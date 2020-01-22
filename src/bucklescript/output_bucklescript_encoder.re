open Graphql_ppx_base;
open Graphql_ast;
open Source_pos;
open Schema;

open Ast_406;
open Asttypes;

open Type_utils;
open Generator_utils;
open Output_bucklescript_utils;
open Extract_type_definitions;

let mangle_enum_name = Generator_utils.uncapitalize_ascii;

let ident_from_string = (~loc=Location.none, ident) =>
  Ast_helper.(Exp.ident(~loc, {txt: Longident.parse(ident), loc}));

module StringSet = Set.Make(String);

let sort_variable_types = (schema, variables) => {
  let recursive_flag = ref(false);
  let ordered_nodes = Queue.create();
  let has_added_to_queue = name =>
    Queue.fold((acc, (_, v)) => acc || name == v, false, ordered_nodes);
  let rec loop = visit_stack =>
    fun
    | [] => ()
    | [(span, type_ref), ...tail] => {
        let type_name = innermost_name(type_ref);
        let () =
          switch (lookup_type(schema, type_name)) {
          | None => ()
          | Some(_) when StringSet.mem(type_name, visit_stack) =>
            recursive_flag := true
          | Some(_) when has_added_to_queue(type_name) => ()
          | Some(Enum(_)) => Queue.push((span, type_name), ordered_nodes)
          | Some(InputObject(io)) =>
            let () =
              loop(
                StringSet.add(type_name, visit_stack),
                io.iom_input_fields
                |> List.map(({am_arg_type, _}) => (span, am_arg_type)),
              );
            Queue.push((span, type_name), ordered_nodes);
          | Some(_) => ()
          };
        loop(visit_stack, tail);
      };

  let () = loop(StringSet.empty, variables);
  let ordered_nodes =
    Array.init(
      Queue.length(ordered_nodes),
      _ => {
        let (span, name) = Queue.take(ordered_nodes);
        (span, name |> lookup_type(schema) |> Option.unsafe_unwrap);
      },
    );
  (recursive_flag^, ordered_nodes);
};

let function_name_string = x =>
  "json_of_" ++ Schema.extract_name_from_type_meta(x);

let rec parser_for_type = (schema, loc, type_ref) => {
  let raise_inconsistent_schema = type_name =>
    raise_error_with_loc(
      loc,
      "Inconsistent schema, type named " ++ type_name ++ " cannot be found",
    );
  switch (type_ref) {
  | Ntr_list(x) =>
    let child_parser = parser_for_type(schema, loc, x);
    [@metaloc conv_loc(loc)]
    [%expr (v => Js.Json.array(Js.Array.map([%e child_parser], v)))];
  | Ntr_nullable(x) =>
    let child_parser = parser_for_type(schema, loc, x);
    [@metaloc conv_loc(loc)]
    [%expr
      (
        v =>
          switch (v) {
          | None => Js.Json.null
          | Some(v) => [%e child_parser](v)
          }
      )
    ];
  | Ntr_named(type_name) =>
    switch (lookup_type(schema, type_name)) {
    | None => raise_inconsistent_schema(type_name)
    | Some(Scalar({sm_name: "String", _}))
    | Some(Scalar({sm_name: "ID", _})) =>
      %expr
      Js.Json.string
    | Some(Scalar({sm_name: "Int", _})) =>
      %expr
      (v => Js.Json.number(float_of_int(v)))
    | Some(Scalar({sm_name: "Float", _})) =>
      %expr
      Js.Json.number
    | Some(Scalar({sm_name: "Boolean", _})) =>
      %expr
      Js.Json.boolean
    | Some(Scalar(_)) =>
      %expr
      (v => v)
    | Some(ty) =>
      function_name_string(ty) |> ident_from_string(~loc=conv_loc(loc))
    }
  };
};

let filter_out_null_values = [%expr
  Js.Array.filter(((_, value)) => !Js.Json.test(value, Js.Json.Null))
];

let json_of_fields = (schema, loc, expr, fields) => {
  let field_array_exprs =
    fields
    |> List.map(({am_name, am_arg_type, _}) => {
         let type_ref = to_native_type_ref(am_arg_type);
         let parser = parser_for_type(schema, loc, type_ref);
         [@metaloc conv_loc(loc)]
         [%expr
           (
             [%e
               Ast_helper.Exp.constant(
                 Parsetree.Pconst_string(am_name, None),
               )
             ],
             [%e parser](
               [%e expr]##[%e ident_from_string(~loc=conv_loc(loc), am_name)],
             ),
           )
         ];
       });
  let field_array = Ast_helper.Exp.array(field_array_exprs);
  [@metaloc conv_loc(loc)]
  [%expr
    Js.Json.object_(
      [%e field_array] |> [%e filter_out_null_values] |> Js.Dict.fromArray,
    )
  ];
};
let generate_encoder = (config, (spanning, x)) => {
  let loc = config.map_loc(spanning.span);
  let body =
    switch (x) {
    | Scalar(_) =>
      raise_error_with_loc(
        loc,
        "Can not build variable encoder for scalar type",
      )
    | Object(_) =>
      raise @@ Invalid_argument("Unsupported variable type: Object")
    | Interface(_) =>
      raise @@ Invalid_argument("Unsupported variable type: Interface")
    | Union(_) =>
      raise @@ Invalid_argument("Unsupported variable type: Union")
    | Enum({em_values, _}) =>
      let match_arms =
        em_values
        |> List.map(({evm_name, _}) => {
             let pattern = Ast_helper.Pat.variant(evm_name, None);
             let expr =
               Ast_helper.Exp.constant(
                 Parsetree.Pconst_string(evm_name, None),
               );
             Ast_helper.Exp.case(pattern, [%expr Js.Json.string([%e expr])]);
           });
      Ast_helper.Exp.match([%expr value], match_arms);
    | InputObject({iom_input_fields, _}) =>
      json_of_fields(config.schema, loc, [%expr value], iom_input_fields)
    };

  let loc = conv_loc(loc);
  Ast_helper.Vb.mk(
    ~loc,
    Ast_helper.Pat.var({txt: function_name_string(x), loc}),
    [%expr value => [%e body]],
  );
};

let generate_encoders = (config, _loc) =>
  fun
  | Some({item, _}) =>
    item
    |> List.map(((span, {vd_type: variable_type, _})) =>
         (span, to_schema_type_ref(variable_type.item))
       )
    |> sort_variable_types(config.schema)
    |> (
      ((is_recursive, types)) => (
        if (is_recursive) {Recursive} else {Nonrecursive},
        Array.map(generate_encoder(config), types),
      )
    )

  | None => (Nonrecursive, [||]);

/*
  * This serializes a variable type to an option type with a JSON value
  * the reason that it generates an option type is that we don't want the values
  * to become Js.Json.null, that would mean actually setting a value to null in
  * the GraphQL spec.
  *
  * What we want however is to remove these values from the generated JSON
  * object. To be able to achieve that we wrap it in an option, so that we know
  * which values to remove.
  *
  * In the future we'd like to support a flag so that:
  *   Some(Some(val)) => actual value
  *   None => not present in JSON object
  *   Some(None) => Null
 */
let rec serialize_type =
  fun
  | Type(Scalar({sm_name: "ID"}))
  | Type(Scalar({sm_name: "String"})) => [%expr Some(Js.Json.string(v))]
  | Type(Scalar({sm_name: "Int"})) => [%expr
      Some(Js.Json.number(float_of_int(v)))
    ]
  | Type(Scalar({sm_name: "Float"})) => [%expr Some(Js.Json.number(v))]
  | Type(Scalar({sm_name: "Boolean"})) => [%expr Some(Js.Json.boolean(v))]
  | Type(Scalar({sm_name: _})) => [%expr Some(v)]
  | Type(InputObject({iom_name})) => [%expr
      Some([%e ident_from_string("serializeInputObject" ++ iom_name)](v))
    ]
  | Type(Enum({em_values})) => {
      let case_exp =
        Ast_helper.(
          Exp.match(
            ident_from_string("v"),
            em_values
            |> List.map(value => {
                 Exp.case(
                   Pat.variant(value.evm_name, None),
                   Exp.apply(
                     ident_from_string("Js.Json.string"),
                     [
                       (
                         Nolabel,
                         Ast_helper.Exp.constant(
                           Parsetree.Pconst_string(value.evm_name, None),
                         ),
                       ),
                     ],
                   ),
                 )
               }),
          )
        );
      %expr
      Some([%e case_exp]);
    }
  | Nullable(inner) =>
    switch%expr (v) {
    | None => None
    | Some(v) =>
      %e
      serialize_type(inner)
    }
  // in this case if there are null values in the list actually convert them to
  // JSON nulls
  | List(inner) => [%expr
      Some(
        v
        |> Array.map(v =>
             switch ([%e serialize_type(inner)]) {
             | Some(v) => v
             | None => Js.Json.null
             }
           ),
      )
    ]
  | Type(Object(_)) => [%expr None]
  | Type(Union(_)) => [%expr None]
  | Type(Interface(_)) => [%expr None]
  | TypeNotFound(_) => [%expr None];

/*
 * This creates a serialize function for variables and/or input types
 * the return type is JSON.
 *
 */
let serialize_fun = fields => {
  let arg = "input";
  Ast_helper.(
    Exp.fun_(
      Nolabel,
      None,
      Pat.var(~loc=Location.none, {txt: arg, loc: Location.none}),
      {
        let field_array =
          fields
          |> List.map(
               fun
               | InputField({name, type_}) => {
                   %expr
                   {
                     let v = [%e ident_from_string(arg ++ "." ++ name)];
                     (
                       [%e
                         Ast_helper.Exp.constant(
                           Parsetree.Pconst_string(name, None),
                         )
                       ],
                       [%e serialize_type(type_)],
                     );
                   };
                 },
             )
          |> Ast_helper.Exp.array;

        %expr
        [%e field_array]
        |> Js.Array.filter(
             fun
             | (_, None) => false
             | (_, Some(_)) => true,
           )
        |> Js.Array.map(
             fun
             | (k, Some(v)) => (k, v)
             | (k, None) => (k, Js.Json.null),
           )
        |> Js.Dict.fromArray
        |> Js.Json.object_;
      },
    )
  );
};

let generate_serialize_variables = (arg_type_defs: list(arg_type_def)) =>
  Ast_helper.(
    Str.value(
      Recursive,
      arg_type_defs
      |> List.map(
           fun
           | InputObject({name, fields, loc}) =>
             Vb.mk(
               Pat.constraint_(
                 Pat.var({
                   loc: Location.none,
                   txt:
                     switch (name) {
                     | None => "serializeVariables"
                     | Some(input_object_name) =>
                       "serializeInputObject" ++ input_object_name
                     },
                 }),
                 Typ.arrow(
                   Nolabel,
                   Typ.constr(
                     {
                       txt:
                         Longident.parse(
                           switch (name) {
                           | None => "t_variables"
                           | Some(input_object_name) =>
                             "t_variables_" ++ input_object_name
                           },
                         ),
                       loc: Location.none,
                     },
                     [],
                   ),
                   Typ.constr(
                     {txt: Longident.parse("Js.Json.t"), loc: Location.none},
                     [],
                   ),
                 ),
               ),
               serialize_fun(fields),
             ),
         ),
    )
  );

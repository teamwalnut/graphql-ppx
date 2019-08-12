open Graphql_ppx_base;
open Graphql_ast;
open Source_pos;
open Schema;

open Ast_406;
open Asttypes;

open Type_utils;
open Generator_utils;
open Output_bucklescript_utils;

let mangle_enum_name = Generator_utils.uncapitalize_ascii;

let ident_from_string = (loc, func_name) =>
  Ast_helper.(Exp.ident(~loc, {txt: Longident.parse(func_name), loc}));

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
      ty |> function_name_string |> ident_from_string(conv_loc(loc))
    }
  };
};

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
               [%e expr]##[%e ident_from_string(conv_loc(loc), am_name)],
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

let generate_encoders = (config, _loc, structure) =>
  switch (structure) {
  | Some({item, _}) =>
    let (is_recursive, types) =
      item
      |> List.map(((span, {vd_type: variable_type, _})) =>
           (span, to_schema_type_ref(variable_type.item))
         )
      |> sort_variable_types(config.schema);

    let rec_flag = if (is_recursive) {Recursive} else {Nonrecursive};

    (
      rec_flag,
      types |> Array.map(generate_encoder(config)) |> Array.to_list,
    );

  | None => (Nonrecursive, [])
  };

open Migrate_parsetree;
open Graphql_ppx_base;
open Graphql_ast;
open Source_pos;
open Schema;

open Ast_408;
open Asttypes;

open Type_utils;
open Generator_utils;
open Output_bucklescript_utils;
open Extract_type_definitions;

let ident_from_string = (~loc=Location.none, ident) =>
  Ast_helper.(Exp.ident(~loc, {txt: Longident.parse(ident), loc}));

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
  | Type(Scalar({sm_name: "String"})) => [%expr
      (a => Some(Js.Json.string(a)))
    ]
  | Type(Scalar({sm_name: "Int"})) => [%expr
      (a => Some(Js.Json.number(float_of_int(a))))
    ]
  | Type(Scalar({sm_name: "Float"})) => [%expr
      (a => Some(Js.Json.number(a)))
    ]
  | Type(Scalar({sm_name: "Boolean"})) => [%expr
      (a => Some(Js.Json.boolean(a)))
    ]
  | Type(Scalar({sm_name: _})) => [%expr (a => Some(a))]
  | Type(InputObject({iom_name})) => [%expr
      (
        a =>
          Some(
            [%e ident_from_string("serializeInputObject" ++ iom_name)](a),
          )
      )
    ]
  | Type(Enum({em_values})) => {
      let case_exp =
        Ast_helper.(
          Exp.match(
            ident_from_string("a"),
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
      (a => Some([%e case_exp]));
    }
  | Nullable(inner) => [%expr
      (
        a =>
          switch (a) {
          | None => None
          | Some(b) => [%e serialize_type(inner)](b)
          }
      )
    ]
  // in this case if there are null values in the list actually convert them to
  // JSON nulls
  | List(inner) => [%expr
      (
        a =>
          Some(
            a
            |> Array.map(b =>
                 switch ([%e serialize_type(inner)](b)) {
                 | Some(c) => c
                 | None => Js.Json.null
                 }
               )
            |> Js.Json.array,
          )
      )
    ]
  | Type(Object(_)) => [%expr (v => None)]
  | Type(Union(_)) => [%expr (v => None)]
  | Type(Interface(_)) => [%expr (v => None)]
  | TypeNotFound(_) => [%expr (v => None)];

/*
 * This creates a serialize function for variables and/or input types
 * the return type is Js.Json.t.
 */
let serialize_fun = (config, fields) => {
  let arg = "inp";
  Ast_helper.(
    Exp.fun_(
      Nolabel,
      None,
      Pat.var(~loc=Location.none, {txt: arg, loc: Location.none}),
      {
        let field_array =
          fields
          |> List.map((InputField({name, type_, loc})) => {
               %expr
               {
                 (
                   [%e
                     Ast_helper.Exp.constant(
                       Parsetree.Pconst_string(name, None),
                     )
                   ],
                   [%e serialize_type(type_)](
                     switch%e (config.records) {
                     | true =>
                       Exp.field(
                         ident_from_string(arg),
                         {
                           Location.txt: Longident.Lident(name),
                           loc: conv_loc(loc),
                         },
                       )
                     | false =>
                       %expr
                       [%e ident_from_string(arg)]##[%e
                                                       ident_from_string(name)
                                                     ]
                     },
                   ),
                 );
               }
             })
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

let is_recursive = input_objects => {
  List.length(input_objects) > 1;
};

let generate_serialize_variables =
    (config, arg_type_defs: list(arg_type_def)) =>
  switch (arg_type_defs) {
  | [] => None
  | _ =>
    Some(
      Ast_helper.(
        Str.value(
          is_recursive(arg_type_defs) ? Recursive : Nonrecursive,
          arg_type_defs
          |> List.map((InputObject({name, fields, loc})) =>
               [@metaloc conv_loc(loc)]
               Vb.mk(
                 Pat.constraint_(
                   Pat.var({
                     loc: conv_loc(loc),
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
                         loc: conv_loc(loc),
                       },
                       [],
                     ),
                     Typ.constr(
                       {
                         txt: Longident.parse("Js.Json.t"),
                         loc: conv_loc(loc),
                       },
                       [],
                     ),
                   ),
                 ),
                 serialize_fun(config, fields),
               )
             ),
        )
      ),
    )
  };

/*
  * Generate constructors for variables and for input types.
  * If there are lots of optional variables this will generate a function with
  * optional arguments, so you do not have to specify all variables if most are
  * None.
  *
  * This also helps if you don't want the build to break if a optional variable
  * is added.
  *
  * The makeVariables (root) yields Js.Json.t, the input types will yield
  * unserialized contents, but if you use them inside of the makeVariables
  * function, the end-result will be serialized.
 */
let generate_variable_constructors =
    (config, arg_type_defs: list(arg_type_def)) => {
  switch (arg_type_defs) {
  | [] => None
  | _ =>
    Some(
      Ast_helper.(
        Str.value(
          Nonrecursive,
          arg_type_defs
          |> List.map((InputObject({name, fields, loc})) =>
               [@metaloc conv_loc(loc)]
               Vb.mk(
                 Pat.var({
                   loc: conv_loc(loc),
                   txt:
                     switch (name) {
                     | None => "makeVar"
                     | Some(input_object_name) =>
                       "makeInputObject" ++ input_object_name
                     },
                 }),
                 {
                   let rec make_labeled_fun = body =>
                     fun
                     | [] =>
                       [@metaloc loc |> conv_loc] [%expr (() => [%e body])]
                     | [InputField({name, loc, type_}), ...tl] => {
                         let name_loc = loc |> conv_loc;
                         Ast_helper.(
                           Exp.fun_(
                             ~loc=name_loc,
                             switch (type_) {
                             | List(_)
                             | Type(_) => Labelled(name)
                             | _ => Optional(name)
                             },
                             None,
                             Pat.var(
                               ~loc=name_loc,
                               {txt: name, loc: name_loc},
                             ),
                             make_labeled_fun(body, tl),
                           )
                         );
                       };

                   let make_labeled_fun_with_f = (body, fields) => {
                     Ast_helper.(
                       Exp.fun_(
                         ~loc=loc |> conv_loc,
                         Labelled("f"),
                         None,
                         Pat.var(
                           ~loc=loc |> conv_loc,
                           {txt: "f", loc: loc |> conv_loc},
                         ),
                         make_labeled_fun([%expr f([%e body])], fields),
                       )
                     );
                   };

                   let record =
                     Ast_helper.(
                       Exp.record(
                         ~loc=loc |> conv_loc,
                         fields
                         |> List.map((InputField({name, loc})) =>
                              (
                                {
                                  Location.txt: Longident.parse(name),
                                  loc: conv_loc(loc),
                                },
                                ident_from_string(name),
                              )
                            ),
                         None,
                       )
                     );

                   let object_ =
                     Ast_408.(
                       Ast_helper.(
                         Exp.extension((
                           {txt: "bs.obj", loc: conv_loc(loc)},
                           PStr([[%stri [%e record]]]),
                         ))
                       )
                     );

                   let body =
                     Ast_helper.(
                       Exp.constraint_(
                         config.records ? record : object_,
                         Typ.constr(
                           {
                             txt:
                               Longident.parse(
                                 switch (name) {
                                 | None => "t_variables"
                                 | Some(input_type_name) =>
                                   "t_variables_" ++ input_type_name
                                 },
                               ),
                             loc: conv_loc(loc),
                           },
                           [],
                         ),
                       )
                     );

                   switch (name) {
                   | None =>
                     Ast_helper.(
                       make_labeled_fun_with_f(
                         Exp.apply(
                           Exp.ident({
                             Location.txt:
                               Longident.Lident("serializeVariables"),
                             loc: conv_loc(loc),
                           }),
                           [(Nolabel, body)],
                         ),
                         fields,
                       )
                     )

                   | Some(_) => make_labeled_fun(body, fields)
                   };
                 },
               )
             ),
        )
      ),
    )
  };
};

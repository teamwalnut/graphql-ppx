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
      (v => Some(Js.Json.string(v)))
    ]
  | Type(Scalar({sm_name: "Int"})) => [%expr
      (v => Some(Js.Json.number(float_of_int(v))))
    ]
  | Type(Scalar({sm_name: "Float"})) => [%expr
      (v => Some(Js.Json.number(v)))
    ]
  | Type(Scalar({sm_name: "Boolean"})) => [%expr
      (v => Some(Js.Json.boolean(v)))
    ]
  | Type(Scalar({sm_name: _})) => [%expr (v => Some(v))]
  | Type(InputObject({iom_name})) => [%expr
      (
        v =>
          Some(
            [%e ident_from_string("serializeInputObject" ++ iom_name)](v),
          )
      )
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
      (v => Some([%e case_exp]));
    }
  | Nullable(inner) => [%expr
      (
        v =>
          switch (v) {
          | None => None
          | Some(v) => [%e serialize_type(inner)](v)
          }
      )
    ]
  // in this case if there are null values in the list actually convert them to
  // JSON nulls
  | List(inner) => [%expr
      (
        v =>
          Some(
            v
            |> Array.map(v =>
                 switch ([%e serialize_type(inner)](v)) {
                 | Some(v) => v
                 | None => Js.Json.null
                 }
               ),
          )
      )
    ]
  | Type(Object(_)) => [%expr (v => None)]
  | Type(Union(_)) => [%expr (v => None)]
  | Type(Interface(_)) => [%expr (v => None)]
  | TypeNotFound(_) => [%expr (v => None)];

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
                     (
                       [%e
                         Ast_helper.Exp.constant(
                           Parsetree.Pconst_string(name, None),
                         )
                       ],
                       [%e serialize_type(type_)](
                         [%e ident_from_string(arg ++ "." ++ name)],
                       ),
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

let generate_variable_constructors =
    (schema, arg_type_defs: list(arg_type_def)) => {
  Ast_helper.(
    Str.value(
      Nonrecursive,
      arg_type_defs
      |> List.map(
           fun
           | InputObject({name, fields, loc}) =>
             Vb.mk(
               Pat.var({
                 loc: Location.none,
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
                   | [] => [@metaloc loc |> conv_loc] [%expr (() => [%e body])]
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

                 let body =
                   Ast_helper.(
                     Exp.constraint_(
                       Exp.record(
                         ~loc=loc |> conv_loc,
                         fields
                         |> List.map(
                              fun
                              | InputField({name, loc}) => (
                                  {
                                    Location.txt: Longident.parse(name),
                                    loc: Location.none,
                                  },
                                  ident_from_string(name),
                                ),
                            ),
                         None,
                       ),
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
                           loc: Location.none,
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
                           loc: Location.none,
                         }),
                         [(Nolabel, body)],
                       ),
                       fields,
                     )
                   )

                 | Some(_) => make_labeled_fun(body, fields)
                 };
               },
             ),
         ),
    )
  );
};

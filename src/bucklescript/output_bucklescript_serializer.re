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
open Result_structure;
open Output_bucklescript_types;

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
                     Ast_helper.(
                       Exp.extension((
                         {txt: "bs.obj", loc: conv_loc(loc)},
                         PStr([[%stri [%e record]]]),
                       ))
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

let base_type_name = name =>
  Ast_helper.(
    Typ.constr({txt: Longident.parse(name), loc: Location.none}, [])
  );
let raw_value = loc => [@metaloc loc] [%expr value];
let const_str_expr = s => Ast_helper.(Exp.constant(Pconst_string(s, None)));
let ident_from_string = (~loc=Location.none, ident) =>
  Ast_helper.(Exp.ident(~loc, {txt: Longident.parse(ident), loc}));

let get_field = (is_object, key, existing_record, path) => {
  is_object
    ? [%expr value##[%e ident_from_string(to_valid_ident(key))]]
    : [%expr
      [%e
        Ast_helper.(
          Exp.field(
            Exp.constraint_(
              ident_from_string("value"),
              Ast_helper.Typ.constr(
                {
                  txt:
                    Longident.parse(
                      switch (existing_record) {
                      | None =>
                        Extract_type_definitions.generate_type_name(path)
                      | Some(existing) => existing
                      },
                    ),
                  loc: Location.none,
                },
                [],
              ),
            ),
            {loc: Location.none, txt: Longident.parse(to_valid_ident(key))},
          )
        )
      ]
    ];
};

let rec generate_nullable_encoder = (config, loc, inner, path, definition) =>
  [@metaloc loc]
  (
    switch%expr (value) {
    | Some(value) =>
      Js.Nullable.return(
        generate_serializer(config, path, definition, inner),
      )
    | None => Js.Nullable.null
    }
  )
and generate_array_encoder = (config, loc, inner, path, definition) =>
  [@metaloc loc]
  [%expr
    value
    |> Js.Array.map(value => {
         %e
         generate_serializer(config, path, definition, inner)
       })
  ]
and generate_poly_enum_encoder = (loc, enum_meta) => {
  let enum_match_arms =
    Ast_helper.(
      enum_meta.em_values
      |> List.map(({evm_name, _}) =>
           Exp.case(Pat.variant(evm_name, None), const_str_expr(evm_name))
         )
    );

  let fallback_arm =
    Ast_helper.(
      Exp.case(
        Pat.variant(
          "FutureAddedValue",
          Some(Pat.var({loc: conv_loc(loc), txt: "other"})),
        ),
        ident_from_string("other"),
      )
    );

  let match_expr =
    Ast_helper.(
      Exp.match(
        [%expr Obj.magic(value: string)],
        List.concat([enum_match_arms, [fallback_arm]]),
      )
    );

  %expr
  [%e match_expr];
}
and generate_custom_encoder = (config, loc, ident, inner, path, definition) =>
  [@metaloc loc]
  {
    %expr
    [%e ident_from_string(ident ++ ".serialize")](
      [%e generate_serializer(config, path, definition, inner)],
    );
  }
and generate_object_encoder =
    (config, loc, name, fields, path, definition, existing_record) => {
  let opaque =
    fields
    |> List.exists(
         fun
         | Fr_fragment_spread(_) => true
         | _ => false,
       );

  let do_obj_constructor_base = is_object => {
    Ast_helper.(
      Exp.record(
        fields
        |> List.fold_left(
             acc =>
               fun
               | Fr_named_field(key, _, inner) => [
                   (
                     {
                       Location.txt: Longident.parse(to_valid_ident(key)),
                       loc,
                     },
                     {
                       let%expr value = [%e
                         get_field(is_object, key, existing_record, path)
                       ];
                       %e
                       generate_serializer(
                         config,
                         [key, ...path],
                         definition,
                         inner,
                       );
                     },
                   ),
                   ...acc,
                 ]
               | Fr_fragment_spread(key, loc, name, _, arguments) => acc,
             [],
           )
        |> List.rev,
        None,
      )
    );
  };

  let do_obj_constructor = () =>
    [@metaloc loc]
    {
      Ast_helper.(
        Exp.extension((
          {txt: "bs.obj", loc},
          PStr([[%stri [%e do_obj_constructor_base(true)]]]),
        ))
      );
    };

  let do_obj_constructor_records = () =>
    [@metaloc loc]
    {
      Ast_helper.(
        Exp.constraint_(
          do_obj_constructor_base(false),
          Ast_helper.Typ.constr(
            {
              txt:
                Longident.parse(
                  "Raw." ++ Extract_type_definitions.generate_type_name(path),
                ),
              loc: Location.none,
            },
            [],
          ),
        )
      );
    };

  let merge_into_opaque = () => {
    let%expr initial: Js.Json.t = Obj.magic([%e do_obj_constructor()]);
    Js.Array.reduce(
      Graphql_PPX.deepMerge,
      initial,
      [%e
        fields
        |> List.fold_left(
             acc =>
               fun
               | Fr_named_field(key, _, inner) => acc
               | Fr_fragment_spread(key, loc, name, _, arguments) => [
                   [%expr
                     [%e ident_from_string(name ++ ".serialize")](
                       [%e get_field(true, key, existing_record, path)],
                     )
                   ],
                   ...acc,
                 ],
             [],
           )
        |> List.rev
        |> Ast_helper.Exp.array
      ],
    );
  };

  opaque
    ? merge_into_opaque()
    : config.records ? do_obj_constructor_records() : do_obj_constructor();
}
and generate_poly_variant_union_encoder =
    (config, loc, name, fragments, exhaustive, path, definition) => {
  let fragment_cases =
    Ast_helper.(
      fragments
      |> List.map(((type_name, inner)) => {
           Ast_helper.(
             Exp.case(
               Pat.variant(
                 type_name,
                 Some(Pat.var({txt: "value", loc: Location.none})),
               ),
               [%expr
                 (
                   Obj.magic(
                     [%e
                       generate_serializer(
                         config,
                         [type_name, ...path],
                         definition,
                         inner,
                       )
                     ],
                   ): [%t
                     base_type_name(
                       "Raw."
                       ++ Extract_type_definitions.generate_type_name(path),
                     )
                   ]
                 )
               ],
             )
           )
         })
    );

  let fallback_case =
    Ast_helper.(
      Exp.case(
        Pat.variant(
          "FutureAddedValue",
          Some(Pat.var({txt: "value", loc: Location.none})),
        ),
        [%expr
          (
            Obj.magic(ident_from_string("value")): [%t
              base_type_name(
                "Raw." ++ Extract_type_definitions.generate_type_name(path),
              )
            ]
          )
        ],
      )
    );

  let typename_matcher =
    Ast_helper.(
      Exp.match(
        [%expr value],
        List.concat([fragment_cases, [fallback_case]]),
      )
    );

  %expr
  [%e typename_matcher];
}
and generate_poly_variant_selection_set_encoder =
    (config, loc, name, fields, path, definition) => [%expr
  Js.Json.null
]
and generate_poly_variant_interface_encoder =
    (config, loc, name, base, fragments, path, definition) => [%expr
  Js.Json.null
]
and generate_solo_fragment_spread_encorder =
    (config, loc, name, arguments, definition) => [%expr
  [%e ident_from_string(name ++ ".serialize")](
    [%e ident_from_string("value")],
  )
]

and generate_error = (loc, message) => {
  let ext = Ast_mapper.extension_of_error(Location.error(~loc, message));
  let%expr _value = value;
  %e
  Ast_helper.Exp.extension(~loc, ext);
}
and generate_serializer = (config, path: list(string), definition) =>
  fun
  | Res_nullable(loc, inner) =>
    generate_nullable_encoder(config, conv_loc(loc), inner, path, definition)
  | Res_array(loc, inner) =>
    generate_array_encoder(config, conv_loc(loc), inner, path, definition)
  | Res_id(loc) => raw_value(conv_loc(loc))
  | Res_string(loc) => raw_value(conv_loc(loc))
  | Res_int(loc) => raw_value(conv_loc(loc))
  | Res_float(loc) => raw_value(conv_loc(loc))
  | Res_boolean(loc) => raw_value(conv_loc(loc))
  | Res_raw_scalar(loc) => raw_value(conv_loc(loc))
  | Res_poly_enum(loc, enum_meta) =>
    generate_poly_enum_encoder(loc, enum_meta)
  | Res_custom_decoder(loc, ident, inner) =>
    generate_custom_encoder(
      config,
      conv_loc(loc),
      ident,
      inner,
      path,
      definition,
    )
  | Res_record(loc, name, fields, existing_record) =>
    generate_object_encoder(
      config,
      conv_loc(loc),
      name,
      fields,
      path,
      definition,
      existing_record,
    )
  | Res_object(loc, name, fields, existing_record) =>
    generate_object_encoder(
      config,
      conv_loc(loc),
      name,
      fields,
      path,
      definition,
      existing_record,
    )
  | Res_poly_variant_union(loc, name, fragments, exhaustive) =>
    generate_poly_variant_union_encoder(
      config,
      conv_loc(loc),
      name,
      fragments,
      exhaustive,
      path,
      definition,
    )
  | Res_poly_variant_selection_set(loc, name, fields) =>
    generate_poly_variant_selection_set_encoder(
      config,
      conv_loc(loc),
      name,
      fields,
      path,
      definition,
    )
  | Res_poly_variant_interface(loc, name, base, fragments) =>
    generate_poly_variant_interface_encoder(
      config,
      conv_loc(loc),
      name,
      base,
      fragments,
      [name, ...path],
      definition,
    )
  | Res_solo_fragment_spread(loc, name, arguments) =>
    generate_solo_fragment_spread_encorder(
      config,
      conv_loc(loc),
      name,
      arguments,
      definition,
    )
  | Res_error(loc, message) => generate_error(conv_loc(loc), message);

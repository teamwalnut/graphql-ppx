open Graphql_ppx_base;
open Migrate_parsetree;
open Source_pos;
open Schema;

open Ast_408;
open Asttypes;

// open Generator_utils;
open Output_bucklescript_utils;
open Extract_type_definitions;
open Result_structure;
open Output_bucklescript_types;

// Cheap way of preventing location override from types module
// Might refactor this later
let conv_loc = _ => Location.none;

let raw_value = loc => [@metaloc loc] [%expr value];

let raw_opaque_object = (interface_fragments, fields) => {
  let has_fragments =
    fields
    |> List.exists(
         fun
         | Fr_fragment_spread(_) => true
         | _ => false,
       );
  switch (has_fragments, interface_fragments) {
  | (true, _) => true
  | (_, Some((_, []))) => false
  | (_, Some((_, _))) => true
  | _ => false
  };
};
let rec list_literal =
  fun
  | [] =>
    Ast_helper.Exp.construct(
      {txt: Longident.Lident("[]"), loc: Location.none},
      None,
    )
  | [value, ...values] => {
      Ast_helper.(
        Exp.construct(
          {txt: Longident.Lident("::"), loc: Location.none},
          Some(Exp.tuple([value, list_literal(values)])),
        )
      );
    };
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
  | Type(Scalar({sm_name: "String"})) =>
    Ppx_config.native() ? [%expr (a => `String(a))] : [%expr (a => a)]
  | Type(Scalar({sm_name: "Int"})) =>
    Ppx_config.native() ? [%expr (a => `Int(a))] : [%expr (a => a)]
  | Type(Scalar({sm_name: "Float"})) =>
    Ppx_config.native() ? [%expr (a => `Float(a))] : [%expr (a => a)]
  | Type(Scalar({sm_name: "Boolean"})) =>
    Ppx_config.native() ? [%expr (a => `Bool(a))] : [%expr (a => a)]
  | Type(Scalar({sm_name: _})) => [%expr (a => a)]
  | Type(InputObject({iom_name})) => [%expr
      (a => [%e ident_from_string("serializeInputObject" ++ iom_name)](a))
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
                   Ppx_config.native()
                     ? [%expr
                       `String(
                         [%e
                           Ast_helper.Exp.constant(
                             Parsetree.Pconst_string(value.evm_name, None),
                           )
                         ],
                       )
                     ]
                     : Ast_helper.Exp.constant(
                         Parsetree.Pconst_string(value.evm_name, None),
                       ),
                 )
               }),
          )
        );
      %expr
      (a => [%e case_exp]);
    }
  | Nullable(inner) =>
    Ppx_config.native()
      ? [%expr
        (
          a =>
            switch (a) {
            // to be fixed (actually we need to drop the value here)
            | None => `Null
            | Some(b) => [%e serialize_type(inner)](b)
            }
        )
      ]
      : [%expr
        (
          a =>
            switch (a) {
            | None => Js.Nullable.undefined
            | Some(b) => Js.Nullable.return([%e serialize_type(inner)](b))
            }
        )
      ]

  // in this case if there are null values in the list actually convert them to
  // JSON nulls
  | List(inner) =>
    Ppx_config.native()
      ? [%expr
        (
          a =>
            `List(
              Array.map(b => {[%e serialize_type(inner)](b)}, a)
              |> Array.to_list,
            )
        )
      ]
      : [%expr (a => Js.Array.map(b => {[%e serialize_type(inner)](b)}, a))]
  | Type(Object(_)) => [%expr (v => None)]
  | Type(Union(_)) => [%expr (v => None)]
  | Type(Interface(_)) => [%expr (v => None)]
  | TypeNotFound(_) => [%expr (v => None)];

let record_to_object = (loc, record) => {
  Ast_helper.(
    Exp.extension((
      {txt: "bs.obj", loc: conv_loc(loc)},
      PStr([[%stri [%e record]]]),
    ))
  );
};

let serialize_fun = (fields, type_name) => {
  let arg = "inp";
  open Ast_helper;
  let object_ =
    Ppx_config.native()
      ? {
        let assoc_fields =
          List.map(
            (InputField({name, type_})) => {
              %expr
              (
                [%e const_str_expr(name)],
                [%e serialize_type(type_)](
                  [%e
                    Exp.field(
                      Exp.constraint_(
                        ident_from_string(arg),
                        base_type_name(type_name),
                      ),
                      {
                        loc: Location.none,
                        Location.txt: Longident.parse(to_valid_ident(name)),
                      },
                    )
                  ],
                ),
              )
            },
            fields,
          );
        %expr
        `Assoc([%e list_literal(assoc_fields)]);
      }
      : Exp.record(
          fields
          |> List.map((InputField({name, type_, loc})) => {
               (
                 {
                   txt: Longident.parse(to_valid_ident(name)),
                   loc: conv_loc(loc),
                 },
                 [%expr
                   [%e serialize_type(type_)](
                     [%e
                       Exp.field(
                         Exp.constraint_(
                           ident_from_string(arg),
                           base_type_name(type_name),
                         ),
                         {
                           loc: Location.none,
                           Location.txt:
                             Longident.parse(to_valid_ident(name)),
                         },
                       )
                     ],
                   )
                 ],
               )
             }),
          None,
        );
  Ast_helper.(
    Exp.fun_(
      Nolabel,
      None,
      Pat.var(~loc=Location.none, {txt: arg, loc: Location.none}),
      object_,
    )
  );
};

let is_recursive = input_objects => {
  List.length(input_objects) > 1;
};

let filter_map = f => {
  let rec aux = accu =>
    fun
    | [] => List.rev(accu)
    | [x, ...l] =>
      switch (f(x)) {
      | None => aux(accu, l)
      | Some(v) => aux([v, ...accu], l)
      };

  aux([]);
};

let generate_serialize_variable_signatures =
    (arg_type_defs: list(arg_type_def)) =>
  switch (arg_type_defs) {
  | [NoVariables] => [%sig: let serializeVariables: unit => Raw.t_variables]
  | arg_type_defs =>
    Ast_helper.(
      arg_type_defs
      |> filter_map(
           fun
           | InputObject({name, loc}) => Some((name, loc))
           | NoVariables => None,
         )
      |> List.map(((name, loc)) => {
           let type_name =
             switch (name) {
             | None => "t_variables"
             | Some(input_object_name) => "t_variables_" ++ input_object_name
             };
           Sig.value(
             Val.mk(
               {
                 loc: conv_loc(loc),
                 txt:
                   switch (name) {
                   | None => "serializeVariables"
                   | Some(input_object_name) =>
                     "serializeInputObject" ++ input_object_name
                   },
               },
               Typ.arrow(
                 ~loc=conv_loc(loc),
                 Nolabel,
                 base_type_name(type_name),
                 base_type_name("Raw." ++ type_name),
               ),
             ),
           );
         })
    )
  };
let generate_serialize_variables = (arg_type_defs: list(arg_type_def)) =>
  switch (arg_type_defs) {
  | [NoVariables] =>
    Ppx_config.native()
      ? [%stri let serializeVariables = () => `Null]
      : [%stri let serializeVariables = () => ()]
  | arg_type_defs =>
    Ast_helper.(
      Str.value(
        is_recursive(arg_type_defs) ? Recursive : Nonrecursive,
        arg_type_defs
        |> filter_map(
             fun
             | InputObject({name, fields, loc}) => Some((name, fields, loc))
             | NoVariables => None,
           )
        |> List.map(((name, fields, loc)) => {
             let type_name =
               switch (name) {
               | None => "t_variables"
               | Some(input_object_name) =>
                 "t_variables_" ++ input_object_name
               };
             [@metaloc conv_loc(loc)]
             Vb.mk(
               Pat.constraint_(
                 ~loc=conv_loc(loc),
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
                   ~loc=conv_loc(loc),
                   Nolabel,
                   base_type_name(type_name),
                   base_type_name("Raw." ++ type_name),
                 ),
               ),
               serialize_fun(fields, type_name),
             );
           }),
      )
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
 */
let generate_variable_constructors = (arg_type_defs: list(arg_type_def)) => {
  switch (arg_type_defs) {
  | [NoVariables] => None
  | _ =>
    Some(
      Ast_helper.(
        Str.value(
          Nonrecursive,
          arg_type_defs
          |> filter_map(
               fun
               | InputObject({name, fields, loc}) =>
                 Some((name, fields, loc))
               | NoVariables => None,
             )
          |> List.map(((name, fields, loc)) => {
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
                         | Type(_) => Labelled(to_valid_ident(name))
                         | _ => Optional(to_valid_ident(name))
                         },
                         None,
                         Pat.var(
                           ~loc=name_loc,
                           {txt: to_valid_ident(name), loc: name_loc},
                         ),
                         make_labeled_fun(body, tl),
                       )
                     );
                   };

               let object_ =
                 //  Ppx_config.native()
                 //    ? {
                 //      let assoc_fields =
                 //        List.map(
                 //          (InputField({name})) => {
                 //            %expr
                 //            (
                 //              [%e const_str_expr(name)],
                 //              [%e ident_from_string(to_valid_ident(name))],
                 //            )
                 //          },
                 //          fields,
                 //        );
                 //      %expr
                 //      `Assoc([%e list_literal(assoc_fields)]);
                 //    }
                 Ast_helper.(
                   Exp.record(
                     ~loc=loc |> conv_loc,
                     fields
                     |> List.map((InputField({name, loc})) =>
                          (
                            {
                              Location.txt:
                                Longident.parse(to_valid_ident(name)),
                              loc: conv_loc(loc),
                            },
                            ident_from_string(to_valid_ident(name)),
                          )
                        ),
                     None,
                   )
                 );

               let body =
                 Ast_helper.(
                   Exp.constraint_(
                     ~loc=conv_loc(loc),
                     object_,
                     base_type_name(
                       switch (name) {
                       | None => "t_variables"
                       | Some(input_type_name) =>
                         "t_variables_" ++ input_type_name
                       },
                     ),
                   )
                 );

               switch (name) {
               | None =>
                 let make_variables_body = make_labeled_fun(body, fields);
                 [(name, loc, make_variables_body)];
               | Some(_) => [(name, loc, make_labeled_fun(body, fields))]
               };
             })
          |> List.concat
          |> List.map(((name, loc, expr)) => {
               [@metaloc conv_loc(loc)]
               Vb.mk(
                 Pat.var({
                   loc: conv_loc(loc),
                   txt:
                     switch (name) {
                     | None => "makeVariables"
                     | Some("make") => "make"
                     | Some(input_object_name) =>
                       "makeInputObject" ++ input_object_name
                     },
                 }),
                 expr,
               )
             }),
        )
      ),
    )
  };
};

let generate_variable_constructor_signatures =
    (arg_type_defs: list(arg_type_def)) => {
  switch (arg_type_defs) {
  | [NoVariables] => []
  | _ =>
    Ast_helper.(
      arg_type_defs
      |> filter_map(
           fun
           | InputObject({name, fields, loc}) => Some((name, fields, loc))
           | NoVariables => None,
         )
      |> List.map(((name, fields, loc)) => {
           let rec make_labeled_fun = final_type => (
             fun
             | [] => final_type
             | [InputField({name, loc, type_}), ...tl] => {
                 Typ.arrow(
                   switch (type_) {
                   | List(_)
                   | Type(_) => Labelled(to_valid_ident(name))
                   | _ => Optional(to_valid_ident(name))
                   },
                   generate_arg_type(~nulls=false, false, loc, type_),
                   make_labeled_fun(final_type, tl),
                 );
               }
           );

           let final_type =
             Typ.arrow(
               Nolabel,
               base_type_name("unit"),
               base_type_name(
                 switch (name) {
                 | None => "t_variables"
                 | Some(input_type_name) => "t_variables_" ++ input_type_name
                 },
               ),
             );

           (name, loc, make_labeled_fun(final_type, fields));
         })
      |> List.map(((name, loc, type_)) => {
           Sig.value(
             Val.mk(
               {
                 loc: conv_loc(loc),
                 txt:
                   switch (name) {
                   | None => "makeVariables"
                   | Some("make") => "make"
                   | Some(input_object_name) =>
                     "makeInputObject" ++ input_object_name
                   },
               },
               type_,
             ),
           )
         })
    )
  };
};

let get_field = (key, existing_record, path) => {
  %expr
  [%e
    Ast_helper.(
      Exp.field(
        Exp.constraint_(
          ident_from_string("value"),
          base_type_name(
            switch (existing_record) {
            | None => generate_type_name(path)
            | Some(existing) => existing
            },
          ),
        ),
        {loc: Location.none, txt: Longident.parse(to_valid_ident(key))},
      )
    )
  ];
};

let rec generate_nullable_encoder = (config, loc, inner, path, definition) =>
  Ppx_config.native()
    ? [@metaloc loc]
      (
        switch%expr (value) {
        | Some(value) =>
          %e
          generate_serializer(config, path, definition, None, inner)
        | None => `Null
        }
      )
    : [@metaloc loc]
      (
        switch%expr (value) {
        | Some(value) =>
          Js.Nullable.return(
            [%e generate_serializer(config, path, definition, None, inner)],
          )
        | None => Js.Nullable.null
        }
      )
and generate_array_encoder = (config, loc, inner, path, definition) =>
  Ppx_config.native()
    ? [@metaloc loc]
      [%expr
        `List(
          value
          |> Array.map(value => {
               %e
               generate_serializer(config, path, definition, None, inner)
             })
          |> Array.to_list,
        )
      ]
    : [@metaloc loc]
      [%expr
        value
        |> Js.Array.map(value => {
             %e
             generate_serializer(config, path, definition, None, inner)
           })
      ]
and generate_poly_enum_encoder = (loc, enum_meta, omit_future_value) => {
  open Ast_helper;
  let enum_match_arms =
    enum_meta.em_values
    |> List.map(({evm_name, _}) =>
         Exp.case(
           Pat.variant(evm_name, None),
           Ppx_config.native()
             ? [%expr `String([%e const_str_expr(evm_name)])]
             : const_str_expr(evm_name),
         )
       );

  let fallback_arm =
    Exp.case(
      Pat.variant("FutureAddedValue", Some(Pat.var({loc, txt: "other"}))),
      Ppx_config.native()
        ? [%expr `String([%e ident_from_string("other")])]
        : ident_from_string("other"),
    );

  let match_expr =
    Exp.match(
      [%expr value],
      List.concat([
        enum_match_arms,
        omit_future_value ? [] : [fallback_arm],
      ]),
    );

  %expr
  [%e match_expr];
}
and generate_custom_encoder = (config, loc, ident, inner, path, definition) =>
  [@metaloc loc]
  {
    let%expr value = [%e ident_from_string(ident ++ ".serialize")](value);
    %e
    generate_serializer(config, path, definition, None, inner);
  }
and generate_object_encoder =
    (
      config,
      loc,
      _name,
      fields,
      path,
      definition,
      existing_record,
      typename,
      interface_fragments,
    ) => {
  open Ast_helper;
  let is_opaque = raw_opaque_object(interface_fragments, fields);
  let do_obj_constructor_base = wrap => {
    switch (
      fields
      |> filter_map(
           fun
           | Fr_fragment_spread(_) => None
           | Fr_named_field({name, type_}) => Some((name, type_)),
         )
    ) {
    | [] => Ppx_config.native() ? [%expr `Assoc([])] : [%expr Js.Dict.empty]
    | fields =>
      let record =
        Exp.record(
          {
            let fields =
              // if the object is part of a union, it gets passed a typename
              // the typename needs to exist on the raw type, because it is used
              // to parse it into a union type. So if this is supplied, and
              // typename is not already explicitly in the fields, add this.
              if (typename != None
                  && !(
                       fields
                       |> List.exists(
                            fun
                            | ("__typename", _) => true
                            | _ => false,
                          )
                     )) {
                [
                  ("__typename", Res_string({loc: conv_loc_from_ast(loc)})),
                  ...fields,
                ];
              } else {
                fields;
              };

            fields
            |> List.map(((key, _inner)) => {
                 let key_value = {
                   Location.txt: Longident.parse(to_valid_ident(key)),
                   loc,
                 };
                 switch (key, typename) {
                 | ("__typename", Some(typename)) => (
                     key_value,
                     const_str_expr(typename),
                   )
                 | _ => (key_value, ident_from_string(to_valid_ident(key)))
                 };
               });
          },
          None,
        );

      let record = wrap ? record_to_object(loc, record) : record;

      let bindings =
        fields
        |> List.map(((key, inner)) =>
             Vb.mk(
               Pat.var({txt: to_valid_ident(key), loc}),
               {
                 // TODO: would be nice to pass the input instead of relying
                 // on a static identifier called `value`
                 let%expr value = [%e get_field(key, existing_record, path)];
                 %e
                 generate_serializer(
                   config,
                   [key, ...path],
                   definition,
                   None,
                   inner,
                 );
               },
             )
           )
        |> List.rev;
      Exp.let_(Nonrecursive, bindings, record);
    };
  };

  let do_json_encoder = () => {
    switch (
      fields
      |> filter_map(
           fun
           | Fr_fragment_spread(_) => None
           | Fr_named_field({name, type_}) => Some((name, type_)),
         )
    ) {
    | [] =>
      %expr
      `Assoc([])
    | fields =>
      let assoc_fields =
        // if the object is part of a union, it gets passed a typename
        // the typename needs to exist on the raw type, because it is used
        // to parse it into a union type. So if this is supplied, and
        // typename is not already explicitly in the fields, add this.
        (
          if (typename != None
              && !(
                   fields
                   |> List.exists(
                        fun
                        | ("__typename", _) => true
                        | _ => false,
                      )
                 )) {
            [
              ("__typename", Res_string({loc: conv_loc_from_ast(loc)})),
              ...fields,
            ];
          } else {
            fields;
          }
        )
        |> List.map(((key, _inner)) => {
             switch (key, typename) {
             | ("__typename", Some(typename)) =>
               %expr
               ("__typename", `String([%e const_str_expr(typename)]))
             | (key, _) =>
               %expr
               (
                 [%e const_str_expr(key)],
                 [%e ident_from_string(to_valid_ident(key))],
               )
             }
           });

      let assoc = [%expr `Assoc([%e list_literal(assoc_fields)])];

      let bindings =
        fields
        |> List.map(((key, inner)) =>
             Vb.mk(
               Pat.var({txt: to_valid_ident(key), loc}),
               {
                 // TODO: would be nice to pass the input instead of relying
                 // on a static identifier called `value`
                 let%expr value = [%e get_field(key, existing_record, path)];
                 %e
                 generate_serializer(
                   config,
                   [key, ...path],
                   definition,
                   None,
                   inner,
                 );
               },
             )
           )
        |> List.rev;
      Exp.let_(Nonrecursive, bindings, assoc);
    };
  };

  let do_obj_constructor = () =>
    [@metaloc conv_loc(loc)]
    {
      do_obj_constructor_base(true);
    };

  let do_obj_constructor_records = () =>
    Ppx_config.native()
      ? do_json_encoder()
      : [@metaloc conv_loc(loc)]
        {
          Exp.constraint_(
            do_obj_constructor_base(false),
            base_type_name("Raw." ++ generate_type_name(path)),
          );
        };

  // the fields and fragments needs to be deeply merged, the fragments in the
  // reason types are separate fields (separated out from the raw output of
  // the query).
  //
  // The fragments are responsible for generating the raw output for themselves
  // using a serialize function per fragment. However when there are regular
  // fields AND (possibly multiple) fragment spreads we need to deeply merge
  // them to reconcile back to the canonical json representation.
  //
  // The deeply merge is done using a runtime function that is supplied with
  // this PPX as the `GraphQL_PPX.deep_merge` function
  //
  // This also brings an important gotcha: if a field is duplicated between
  // fragments, and it is changed in a particular frament, it is only changed
  // in the raw representation if it is merged last. Unfortunately there is not
  // really anything that we can do about this.

  let merge_into_opaque = () => {
    let fields =
      fields
      |> List.fold_left(
           acc =>
             fun
             | Fr_named_field(_) => acc
             | Fr_fragment_spread({key, name}) => [
                 Ppx_config.native()
                   ? [%expr
                     [%e ident_from_string(name ++ ".toJson")](
                       [%e ident_from_string(name ++ ".serialize")](
                         [%e get_field(key, existing_record, path)],
                       ),
                     )
                   ]
                   : [%expr
                     (
                       Obj.magic(
                         [%e ident_from_string(name ++ ".serialize")](
                           [%e get_field(key, existing_record, path)],
                         ),
                       ): Js.Json.t
                     )
                   ],
                 ...acc,
               ],
           [],
         )
      |> List.rev;
    let fields =
      switch (interface_fragments) {
      | None
      | Some((_, [])) => fields
      | Some((interface_name, fragments)) => [
          Ppx_config.native()
            ? {
              let%expr value = [%e get_field("fragment", None, path)];
              %e
              generate_poly_variant_interface_encoder(
                config,
                loc,
                interface_name,
                fragments,
                path,
                definition,
              );
            }
            : {
              let%expr value = [%e get_field("fragment", None, path)];
              (
                Obj.magic(
                  [%e
                    generate_poly_variant_interface_encoder(
                      config,
                      loc,
                      interface_name,
                      fragments,
                      path,
                      definition,
                    )
                  ],
                ): Js.Json.t
              );
            },
          ...fields,
        ]
      };

    Ppx_config.native()
      // no deepmerge for native, just get the last one
      ? [%expr
        Array.fold_left(
          (_, b) => b,
          {
            %e
            do_obj_constructor();
          },
          [%e fields |> Ast_helper.Exp.array],
        )
      ]
      : [%expr
        (
          Obj.magic(
            Js.Array.reduce(
              GraphQL_PPX.deepMerge,
              Obj.magic(
                {
                  %e
                  do_obj_constructor();
                },
              ): Js.Json.t,
              [%e fields |> Ast_helper.Exp.array],
            ),
          ): [%t
            base_type_name("Raw." ++ generate_type_name(path))
          ]
        )
      ];
  };

  is_opaque ? merge_into_opaque() : do_obj_constructor_records();
}
and generate_poly_variant_union_encoder =
    (
      config,
      _loc,
      _name,
      fragments,
      _exhaustive,
      omit_future_value,
      path,
      definition,
    ) => {
  open Ast_helper;
  let fragment_cases =
    fragments
    |> List.map((({item: type_name}: Result_structure.name, inner)) => {
         Ast_helper.(
           Exp.case(
             Pat.variant(
               type_name,
               Some(Pat.var({txt: "value", loc: Location.none})),
             ),
             Ppx_config.native()
               ? generate_serializer(
                   config,
                   [type_name, ...path],
                   definition,
                   Some(type_name),
                   inner,
                 )
               : [%expr
                 (
                   Obj.magic(
                     [%e
                       generate_serializer(
                         config,
                         [type_name, ...path],
                         definition,
                         Some(type_name),
                         inner,
                       )
                     ],
                   ): [%t
                     base_type_name("Raw." ++ generate_type_name(path))
                   ]
                 )
               ],
           )
         )
       });

  let fallback_case =
    Exp.case(
      Pat.variant(
        "FutureAddedValue",
        Some(Pat.var({txt: "value", loc: Location.none})),
      ),
      Ppx_config.native()
        ? [%expr value]
        : [%expr
          (
            Obj.magic(value): [%t
              base_type_name("Raw." ++ generate_type_name(path))
            ]
          )
        ],
    );

  let typename_matcher =
    Exp.match(
      [%expr value],
      List.concat([
        fragment_cases,
        omit_future_value ? [] : [fallback_case],
      ]),
    );

  %expr
  [%e typename_matcher];
}
and generate_poly_variant_selection_set_encoder =
    (_config, _loc, _name, _fields, _path, _definition) =>
  Ppx_config.native() ? [%expr `Null] : [%expr Obj.magic(Js.Json.null)]
and generate_poly_variant_interface_encoder =
    (config, _loc, name, fragments, path, definition) => {
  open Ast_helper;
  let fragment_cases =
    fragments
    |> List.map(((type_name, inner)) => {
         Ast_helper.(
           Exp.case(
             Pat.variant(
               type_name,
               Some(Pat.var({txt: "value", loc: Location.none})),
             ),
             Ppx_config.native()
               ? generate_serializer(
                   config,
                   [type_name, name, ...path],
                   definition,
                   Some(type_name),
                   inner,
                 )
               : [%expr
                 (
                   Obj.magic(
                     [%e
                       generate_serializer(
                         config,
                         [type_name, name, ...path],
                         definition,
                         Some(type_name),
                         inner,
                       )
                     ],
                   ): [%t
                     base_type_name(
                       "Raw." ++ generate_type_name([name, ...path]),
                     )
                   ]
                 )
               ],
           )
         )
       });

  let fallback_case =
    Exp.case(
      Pat.variant("UnspecifiedFragment", Some(Pat.any())),
      Ppx_config.native()
        ? [%expr `Assoc([])]
        : [%expr
          (
            Obj.magic(Js.Dict.empty()): [%t
              base_type_name("Raw." ++ generate_type_name([name, ...path]))
            ]
          )
        ],
    );

  let typename_matcher =
    Exp.match(
      [%expr value],
      List.concat([fragment_cases, [fallback_case]]),
    );

  %expr
  [%e typename_matcher];
}

and generate_solo_fragment_spread_encorder =
    (_config, _loc, name, _arguments, _definition) =>
  Ppx_config.native()
    ? [%expr
      [%e ident_from_string(name ++ ".toJson")](
        [%e ident_from_string(name ++ ".serialize")](
          [%e ident_from_string("value")],
        ),
      )
    ]
    : [%expr
      [%e ident_from_string(name ++ ".serialize")](
        [%e ident_from_string("value")],
      )
    ]

and generate_error = (loc, message) => {
  let loc = Output_bucklescript_utils.conv_loc(loc);
  let ext = Ast_mapper.extension_of_error(Location.error(~loc, message));
  let%expr _value = value;
  %e
  Ast_helper.Exp.extension(~loc, ext);
}
and generate_serializer = (config, path: list(string), definition, typename) =>
  fun
  | Res_nullable({loc, inner}) =>
    generate_nullable_encoder(config, conv_loc(loc), inner, path, definition)
  | Res_array({loc, inner}) =>
    generate_array_encoder(config, conv_loc(loc), inner, path, definition)
  | Res_id({loc}) =>
    Ppx_config.native()
      ? [@metaloc conv_loc(loc)] [%expr `String(value)]
      : raw_value(conv_loc(loc))
  | Res_string({loc}) =>
    Ppx_config.native()
      ? [@metaloc conv_loc(loc)] [%expr `String(value)]
      : raw_value(conv_loc(loc))
  | Res_int({loc}) =>
    Ppx_config.native()
      ? [@metaloc conv_loc(loc)] [%expr `Int(value)]
      : raw_value(conv_loc(loc))
  | Res_float({loc}) =>
    Ppx_config.native()
      ? [@metaloc conv_loc(loc)] [%expr `Float(value)]
      : raw_value(conv_loc(loc))
  | Res_boolean({loc}) =>
    Ppx_config.native()
      ? [@metaloc conv_loc(loc)] [%expr `Bool(value)]
      : raw_value(conv_loc(loc))
  | Res_raw_scalar({loc}) => raw_value(conv_loc(loc))
  | Res_poly_enum({loc, enum_meta, omit_future_value}) =>
    generate_poly_enum_encoder(conv_loc(loc), enum_meta, omit_future_value)
  | Res_custom_decoder({loc, ident, inner}) =>
    generate_custom_encoder(
      config,
      conv_loc(loc),
      ident,
      inner,
      path,
      definition,
    )
  | Res_record({
      loc,
      name,
      fields,
      type_name: existing_record,
      interface_fragments,
    }) =>
    generate_object_encoder(
      config,
      conv_loc(loc),
      name,
      fields,
      path,
      definition,
      existing_record,
      typename,
      interface_fragments,
    )
  | Res_object({
      loc,
      name,
      fields,
      type_name: existing_record,
      interface_fragments,
    }) =>
    generate_object_encoder(
      config,
      conv_loc(loc),
      name,
      fields,
      path,
      definition,
      existing_record,
      typename,
      interface_fragments,
    )
  | Res_poly_variant_union({
      loc,
      name,
      fragments,
      exhaustive,
      omit_future_value,
    }) =>
    generate_poly_variant_union_encoder(
      config,
      conv_loc(loc),
      name,
      fragments,
      exhaustive,
      omit_future_value,
      path,
      definition,
    )
  | Res_poly_variant_selection_set({loc, name, fragments: fields}) =>
    generate_poly_variant_selection_set_encoder(
      config,
      conv_loc(loc),
      name,
      fields,
      path,
      definition,
    )
  | Res_poly_variant_interface({loc, name, fragments}) =>
    generate_poly_variant_interface_encoder(
      config,
      conv_loc(loc),
      name,
      fragments,
      path,
      definition,
    )
  | Res_solo_fragment_spread({loc, name, arguments}) =>
    generate_solo_fragment_spread_encorder(
      config,
      conv_loc(loc),
      name,
      arguments,
      definition,
    )
  | Res_error({loc, message}) => generate_error(loc, message);

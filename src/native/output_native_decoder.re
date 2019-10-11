open Migrate_parsetree;
open Graphql_ppx_base;
open Result_structure;
open Schema;

open Ast_406;
open Asttypes;
open Parsetree;

open Generator_utils;
open Output_native_utils;

let const_str_expr = s =>
  Ast_helper.(Exp.constant([@implicit_arity] Pconst_string(s, None)));

let make_error_raiser = message =>
  if (Ppx_config.verbose_error_handling()) {
    %expr
    raise(Failure("graphql_ppx: " ++ [%e message]));
  } else {
    %expr
    raise(Failure("Unexpected GraphQL query response"));
  };

let string_decoder = loc =>
  [@metaloc loc]
  (
    switch%expr (value) {
    | `String(value) => value
    | _ =>
      %e
      make_error_raiser(
        [%expr "Expected string, got " ++ Yojson.Basic.to_string(value)],
      )
    }
  );

let id_decoder = string_decoder;

let float_decoder = loc =>
  [@metaloc loc]
  (
    switch%expr (value) {
    | `Float(value) => value
    | `Int(value) => float_of_int(value)
    | _ =>
      %e
      make_error_raiser(
        [%expr "Expected float, got " ++ Yojson.Basic.to_string(value)],
      )
    }
  );

let int_decoder = loc =>
  [@metaloc loc]
  (
    switch%expr (value) {
    | `Int(value) => value
    | _ =>
      %e
      make_error_raiser(
        [%expr "Expected int, got " ++ Yojson.Basic.to_string(value)],
      )
    }
  );

let boolean_decoder = loc =>
  [@metaloc loc]
  (
    switch%expr (value) {
    | `Bool(value) => value
    | _ =>
      %e
      make_error_raiser(
        [%expr "Expected boolean, got " ++ Yojson.Basic.to_string(value)],
      )
    }
  );

let generate_poly_enum_decoder = (loc, enum_meta) => {
  let enum_match_arms =
    Ast_helper.(
      List.map(
        ({evm_name, _}) =>
          Exp.case(
            Pat.constant([@implicit_arity] Pconst_string(evm_name, None)),
            Exp.variant(evm_name, None),
          ),
        enum_meta.em_values,
      )
    );
  let fallback_arm =
    Ast_helper.(
      Exp.case(
        Pat.any(),
        make_error_raiser(
          [%expr
            "Unknown enum variant for "
            ++ [%e const_str_expr(enum_meta.em_name)]
            ++ ": "
            ++ value
          ],
        ),
      )
    );
  let match_expr =
    Ast_helper.(
      Exp.match(
        [%expr value],
        List.concat([enum_match_arms, [fallback_arm]]),
      )
    );
  let enum_ty =
    [@metaloc loc]
    Ast_helper.(
      Typ.variant(
        List.map(
          ({evm_name, _}) =>
            [@implicit_arity] Rtag({txt: evm_name, loc}, [], true, []),
          enum_meta.em_values,
        ),
        Closed,
        None,
      )
    );

  switch%expr (value) {
  | `String(value) => ([%e match_expr]: [%t enum_ty])
  | _ =>
    %e
    make_error_raiser(
      [%expr
        "Expected enum value for "
        ++ [%e const_str_expr(enum_meta.em_name)]
        ++ ", got "
        ++ Yojson.Basic.to_string(value)
      ],
    )
  };
};

let generate_solo_fragment_spread = (loc, name) => {
  let ident =
    Ast_helper.Exp.ident({loc, txt: Longident.parse(name ++ ".parse")});
  %expr
  [%e ident](value);
};

let generate_error = (loc, message) => {
  let ext = Ast_mapper.extension_of_error(Location.error(~loc, message));
  let%expr _value = value;
  %e
  Ast_helper.Exp.extension(~loc, ext);
};

let rec generate_decoder = config =>
  fun
  | [@implicit_arity] Res_nullable(loc, inner) =>
    generate_nullable_decoder(config, conv_loc(loc), inner)
  | [@implicit_arity] Res_array(loc, inner) =>
    generate_array_decoder(config, conv_loc(loc), inner)
  | Res_id(loc) => id_decoder(conv_loc(loc))
  | Res_string(loc) => string_decoder(conv_loc(loc))
  | Res_int(loc) => int_decoder(conv_loc(loc))
  | Res_float(loc) => float_decoder(conv_loc(loc))
  | Res_boolean(loc) => boolean_decoder(conv_loc(loc))
  | Res_raw_scalar(_loc) => {
      %expr
      value;
    }
  | [@implicit_arity] Res_poly_enum(loc, enum_meta) =>
    generate_poly_enum_decoder(conv_loc(loc), enum_meta)
  | [@implicit_arity] Res_custom_decoder(loc, ident, inner) =>
    generate_custom_decoder(config, conv_loc(loc), ident, inner)
  | [@implicit_arity] Res_record(loc, name, fields) =>
    generate_record_decoder(config, conv_loc(loc), name, fields)
  | [@implicit_arity] Res_object(loc, name, fields) =>
    generate_object_decoder(config, conv_loc(loc), name, fields)
  | [@implicit_arity] Res_poly_variant_selection_set(loc, name, fields) =>
    generate_poly_variant_selection_set(config, conv_loc(loc), name, fields)
  | [@implicit_arity] Res_poly_variant_union(loc, name, fragments, exhaustive) =>
    generate_poly_variant_union(
      config,
      conv_loc(loc),
      name,
      fragments,
      exhaustive,
    )
  | [@implicit_arity] Res_poly_variant_interface(loc, name, base, fragments) =>
    generate_poly_variant_interface(
      config,
      conv_loc(loc),
      name,
      base,
      fragments,
    )
  | [@implicit_arity] Res_solo_fragment_spread(loc, name) =>
    generate_solo_fragment_spread(conv_loc(loc), name)
  | [@implicit_arity] Res_error(loc, message) =>
    generate_error(conv_loc(loc), message)
and generate_nullable_decoder = (config, loc, inner) =>
  [@metaloc loc]
  (
    switch%expr (value) {
    | `Null => None
    | value => Some([%e generate_decoder(config, inner)])
    }
  )
and generate_array_decoder = (config, loc, inner) =>
  [@metaloc loc]
  (
    switch%expr (value) {
    | `List(value) =>
      List.map(
        value => {
          let _ = ();
          %e
          generate_decoder(config, inner);
        },
        value,
      )
      |> Array.of_list
    | _ =>
      %e
      make_error_raiser(
        [%expr "Expected array, got " ++ Yojson.Basic.to_string(value)],
      )
    }
  )
and generate_custom_decoder = (config, loc, ident, inner) => {
  let fn_expr =
    Ast_helper.(
      Exp.ident({loc: Location.none, txt: Longident.parse(ident)})
    );
  [@metaloc loc] [%expr [%e fn_expr]([%e generate_decoder(config, inner)])];
}
and generate_record_decoder = (config, loc, name, fields) => {
  /*
     Given a selection set, this function first generates the resolvers,
     binding the results to individual local variables. It then generates
     a record containing each field bound to the corresponding variable.

     While this might seem a bit convoluted, it lets us wrap the record in
     a [%bs.obj ] extension to generate a JavaScript object without having
     to worry about the record-in-javascript-object problem that BuckleScript has.

     As an example, a selection set like "{ f1 f2 f3 }" will result in
     let (field_f1, field_f2, field_f3) = (
       match Js.Dict.get value "f1" with ... end
       match Js.Dict.get value "f2" with ... end
       match Js.Dict.get value "f3" with ... end
     ) in { f1 = field_f1; f2 = field_f2; f3 = field_f3 }
   */

  let field_name_tuple_pattern =
    Ast_helper.(
      fields
      |> filter_map(
           fun
           | [@implicit_arity] Fr_named_field(field, _, _) =>
             Some(Pat.var({loc, txt: "field_" ++ field}))
           | Fr_fragment_spread(_) => None,
         )
      |> Pat.tuple
    );

  let field_decoder_tuple =
    Ast_helper.(
      fields
      |> filter_map(
           fun
           | [@implicit_arity] Fr_named_field(field, loc, inner) => {
               let loc = conv_loc(loc);
               [@metaloc loc]
               Some(
                 switch%expr (List.assoc([%e const_str_expr(field)], value)) {
                 | value =>
                   %e
                   generate_decoder(config, inner)
                 | exception Not_found =>
                   if%e (can_be_absent_as_field(inner)) {
                     %expr
                     None;
                   } else {
                     make_error_raiser(
                       [%expr
                         "Field "
                         ++ [%e const_str_expr(field)]
                         ++ " on type "
                         ++ [%e const_str_expr(name)]
                         ++ " is missing"
                       ],
                     );
                   }
                 },
               );
             }
           | Fr_fragment_spread(_) => None,
         )
      |> Exp.tuple
    );

  let record_fields =
    Ast_helper.(
      fields
      |> List.map(
           fun
           | [@implicit_arity] Fr_named_field(field, loc, _) => {
               let loc = conv_loc(loc);
               (
                 {Location.loc, txt: Longident.Lident(field)},
                 Exp.ident(
                   ~loc,
                   {loc, txt: Longident.Lident("field_" ++ field)},
                 ),
               );
             }
           | [@implicit_arity] Fr_fragment_spread(field, loc, name) => {
               let loc = conv_loc(loc);
               (
                 {Location.loc, txt: Longident.Lident(field)},
                 [@metaloc loc]
                 {
                   let%expr value = `Assoc(value);
                   %e
                   generate_solo_fragment_spread(loc, name);
                 },
               );
             },
         )
    );
  let record = Ast_helper.Exp.record(~loc, record_fields, None);

  switch%expr (value) {
  | `Assoc(value) =>
    let [%p field_name_tuple_pattern] = [%e field_decoder_tuple];
    %e
    record;
  | value =>
    %e
    make_error_raiser(
      [%expr
        "Expected object of type "
        ++ [%e const_str_expr(name)]
        ++ ", got "
        ++ Yojson.Basic.to_string(value)
      ],
    )
  };
}
and generate_object_decoder = (config, loc, name, fields) =>
  [@metaloc loc]
  (
    switch%expr (value) {
    | `Assoc(value) =>
      %e
      Ast_helper.(
        Exp.object_(
          Cstr.mk(
            Pat.any(),
            List.map(
              fun
              | [@implicit_arity] Fr_named_field(key, _, inner) =>
                Cf.method(
                  {txt: key, loc: Location.none},
                  Public,
                  [@implicit_arity]
                  Cfk_concrete(
                    Fresh,
                    switch%expr (List.assoc([%e const_str_expr(key)], value)) {
                    | value =>
                      %e
                      generate_decoder(config, inner)
                    | exception Not_found =>
                      if%e (can_be_absent_as_field(inner)) {
                        %expr
                        None;
                      } else {
                        make_error_raiser(
                          [%expr
                            "Field "
                            ++ [%e const_str_expr(key)]
                            ++ " on type "
                            ++ [%e const_str_expr(name)]
                            ++ " is missing"
                          ],
                        );
                      }
                    },
                  ),
                )
              | [@implicit_arity] Fr_fragment_spread(key, loc, name) => {
                  let loc = conv_loc(loc);
                  Cf.method(
                    {txt: key, loc: Location.none},
                    Public,
                    [@implicit_arity]
                    Cfk_concrete(
                      Fresh,
                      {
                        let%expr value = `Assoc(value);
                        %e
                        generate_solo_fragment_spread(loc, name);
                      },
                    ),
                  );
                },
              fields,
            ),
          ),
        )
      )
    | _ =>
      %e
      make_error_raiser([%expr "Object is not a value"])
    }
  )
and generate_poly_variant_selection_set = (config, loc, name, fields) => {
  let rec generator_loop =
    fun
    | [(field, inner), ...next] => {
        let variant_decoder =
          Ast_helper.(
            Exp.variant(
              Compat.capitalize_ascii(field),
              Some(generate_decoder(config, inner)),
            )
          );
        switch%expr (List.assoc([%e const_str_expr(field)], value)) {
        | exception Not_found =>
          %e
          make_error_raiser(
            [%expr
              "Field "
              ++ [%e const_str_expr(field)]
              ++ " on type "
              ++ [%e const_str_expr(name)]
              ++ " is missing"
            ],
          )
        | temp =>
          switch (temp) {
          | `Null =>
            %e
            generator_loop(next)
          | _ =>
            let value = temp;
            %e
            variant_decoder;
          }
        };
      }
    | [] =>
      make_error_raiser(
        [%expr
          "All fields on variant selection set on type "
          ++ [%e const_str_expr(name)]
          ++ " were null"
        ],
      );
  let variant_type =
    Ast_helper.(
      Typ.variant(
        List.map(
          ((name, _)) =>
            [@implicit_arity]
            Rtag(
              {txt: Compat.capitalize_ascii(name), loc},
              [],
              false,
              [
                {
                  ptyp_desc: Ptyp_any,
                  ptyp_attributes: [],
                  ptyp_loc: Location.none,
                },
              ],
            ),
          fields,
        ),
        Closed,
        None,
      )
    );
  [@metaloc loc]
  (
    switch%expr (value) {
    | `Assoc(value) => ([%e generator_loop(fields)]: [%t variant_type])
    | _ =>
      %e
      make_error_raiser(
        [%expr
          "Expected type " ++ [%e const_str_expr(name)] ++ " to be an object"
        ],
      )
    }
  );
}
and generate_poly_variant_interface = (config, loc, name, base, fragments) => {
  let map_fallback_case = ((type_name, inner)) => {
    open Ast_helper;
    let name_pattern = Pat.any();
    let variant =
      Exp.variant(type_name, Some(generate_decoder(config, inner)));
    Exp.case(name_pattern, variant);
  };

  let map_case = ((type_name, inner)) => {
    open Ast_helper;
    let name_pattern =
      Pat.constant([@implicit_arity] Pconst_string(type_name, None));
    let variant =
      Exp.variant(type_name, Some(generate_decoder(config, inner)));
    Exp.case(name_pattern, variant);
  };
  let map_case_ty = ((name, _)) =>
    [@implicit_arity]
    Rtag(
      name,
      [],
      false,
      [{ptyp_desc: Ptyp_any, ptyp_attributes: [], ptyp_loc: Location.none}],
    );

  let fragment_cases = List.map(map_case, fragments);
  let fallback_case = map_fallback_case(base);
  let (base_name, base_decoder) = base;
  let fallback_case_ty = map_case_ty(({txt: base_name, loc}, base_decoder));

  let fragment_case_tys =
    List.map(
      map_case_ty,
      fragments |> List.map(((key, ty)) => ({txt: key, loc}, ty)),
    );
  let interface_ty =
    Ast_helper.(
      Typ.variant([fallback_case_ty, ...fragment_case_tys], Closed, None)
    );
  let typename_matcher =
    Ast_helper.(
      Exp.match(
        [%expr typename],
        List.concat([fragment_cases, [fallback_case]]),
      )
    );
  [@metaloc loc]
  (
    switch%expr (value) {
    | `Assoc(typename_obj) =>
      switch (List.assoc("__typename", typename_obj)) {
      | exception Not_found =>
        %e
        make_error_raiser(
          [%expr
            "Interface implementation"
            ++ [%e const_str_expr(name)]
            ++ " is missing the __typename field"
          ],
        )
      | typename =>
        switch (typename) {
        | `String(typename) => ([%e typename_matcher]: [%t interface_ty])
        | _ =>
          %e
          make_error_raiser(
            [%expr
              "Interface implementation "
              ++ [%e const_str_expr(name)]
              ++ " has a __typename field that is not a string"
            ],
          )
        }
      }
    | _ =>
      %e
      make_error_raiser(
        [%expr
          "Expected Interface implementation "
          ++ [%e const_str_expr(name)]
          ++ " to be an object, got "
          ++ Yojson.Basic.to_string(value)
        ],
      )
    }
  );
}
and generate_poly_variant_union =
    (config, loc, name, fragments, exhaustive_flag) => {
  let fragment_cases =
    Ast_helper.(
      fragments
      |> List.map(((type_name, inner)) => {
           let name_pattern =
             Pat.constant([@implicit_arity] Pconst_string(type_name, None));
           let variant =
             Ast_helper.(
               Exp.variant(type_name, Some(generate_decoder(config, inner)))
             );
           Exp.case(name_pattern, variant);
         })
    );
  let (fallback_case, fallback_case_ty) =
    Ast_helper.(
      switch (exhaustive_flag) {
      | Result_structure.Exhaustive => (
          Exp.case(
            Pat.var({loc: Location.none, txt: "typename"}),
            make_error_raiser(
              [%expr
                "Union "
                ++ [%e const_str_expr(name)]
                ++ " returned unknown type "
                ++ typename
              ],
            ),
          ),
          [],
        )
      | Nonexhaustive => (
          Exp.case(Pat.any(), [%expr `Nonexhaustive]),
          [
            [@implicit_arity] Rtag({txt: "Nonexhaustive", loc}, [], true, []),
          ],
        )
      }
    );
  let fragment_case_tys =
    List.map(
      ((name, _)) =>
        [@implicit_arity]
        Rtag(
          {txt: name, loc},
          [],
          false,
          [
            {
              ptyp_desc: Ptyp_any,
              ptyp_attributes: [],
              ptyp_loc: Location.none,
            },
          ],
        ),
      fragments,
    );
  let union_ty =
    Ast_helper.(
      Typ.variant(
        List.concat([fallback_case_ty, fragment_case_tys]),
        Closed,
        None,
      )
    );
  let typename_matcher =
    Ast_helper.(
      Exp.match(
        [%expr typename],
        List.concat([fragment_cases, [fallback_case]]),
      )
    );
  [@metaloc loc]
  (
    switch%expr (value) {
    | `Assoc(typename_obj) =>
      switch (List.assoc("__typename", typename_obj)) {
      | exception Not_found =>
        %e
        make_error_raiser(
          [%expr
            "Union "
            ++ [%e const_str_expr(name)]
            ++ " is missing the __typename field"
          ],
        )
      | typename =>
        switch (typename) {
        | `String(typename) => ([%e typename_matcher]: [%t union_ty])
        | _ =>
          %e
          make_error_raiser(
            [%expr
              "Union "
              ++ [%e const_str_expr(name)]
              ++ " has a __typename field that is not a string"
            ],
          )
        }
      }
    | _ =>
      %e
      make_error_raiser(
        [%expr
          "Expected union "
          ++ [%e const_str_expr(name)]
          ++ " to be an object, got "
          ++ Yojson.Basic.to_string(value)
        ],
      )
    }
  );
};

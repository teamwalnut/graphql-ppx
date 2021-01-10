open Migrate_parsetree;
open Graphql_ppx_base;
open Result_structure;
open Schema;

open Ast_408;
open Asttypes;
open Parsetree;

open Generator_utils;
open Output_bucklescript_utils;
open Output_bucklescript_types;
open Extract_type_definitions;

// Cheap way of preventing location override from types module
// Might refactor this later
let conv_loc = _ => Location.none;

let record_to_object = (loc, record) => {
  Ast_helper.(
    Exp.extension((
      {txt: "bs.obj", loc: conv_loc(loc)},
      PStr([[%stri [%e record]]]),
    ))
  );
};

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

let rec generate_poly_type_ref_name = (type_ref: Graphql_ast.type_ref) => {
  switch (type_ref) {
  | Tr_named({item: name}) => name
  | Tr_list({item: type_ref}) =>
    "ListOf_" ++ generate_poly_type_ref_name(type_ref)
  | Tr_non_null_named({item: name}) => "NonNull_" ++ name
  | Tr_non_null_list({item: type_ref}) =>
    "NonNullListOf_" ++ generate_poly_type_ref_name(type_ref)
  };
};

let get_variable_definitions = (definition: Graphql_ast.definition) => {
  switch (definition) {
  | Fragment({item: {fg_variable_definitions: Some({item: definitions})}})
  | Operation({item: {o_variable_definitions: Some({item: definitions})}}) =>
    Graphql_ast.(
      definitions
      |> List.fold_left(
           (
             acc,
             (
               {Source_pos.item: name, span},
               {vd_type: {item: type_ref, span: type_span}},
             ),
           ) =>
             [
               (name, generate_poly_type_ref_name(type_ref), span, type_span),
               ...acc,
             ],
           [],
         )
    )
  | _ => []
  };
};

let make_error_raiser = message =>
  if (Ppx_config.native()) {
    if (Ppx_config.verbose_error_handling()) {
      %expr
      raise(Failure("graphql-ppx: " ++ [%e message]));
    } else {
      %expr
      raise(Failure("Unexpected GraphQL query response"));
    };
  } else if (Ppx_config.verbose_error_handling()) {
    %expr
    Js.Exn.raiseError("graphql-ppx: " ++ [%e message]);
  } else {
    %expr
    Js.Exn.raiseError("Unexpected GraphQL query response");
  };

let raw_value = loc => [@metaloc loc] [%expr value];

let generate_poly_enum_decoder = (loc, enum_meta, omit_future_value) => {
  let enum_match_arms =
    Ast_helper.(
      enum_meta.em_values
      |> List.map(({evm_name, _}) =>
           Exp.case(
             const_str_pat(evm_name),
             Exp.variant(to_valid_ident(evm_name), None),
           )
         )
    );

  let fallback_arm =
    omit_future_value
      ? Ast_helper.(
          Exp.case(
            Pat.any(),
            Exp.apply(
              Exp.ident(Location.mknoloc(Longident.parse("raise"))),
              [
                (
                  Nolabel,
                  Exp.construct(
                    Location.mknoloc(Longident.parse("Not_found")),
                    None,
                  ),
                ),
              ],
            ),
          )
        )
      : Ast_helper.(
          Exp.case(
            Pat.var({loc: conv_loc(loc), txt: "other"}),
            Exp.variant(
              "FutureAddedValue",
              Some(
                Exp.ident({
                  Location.txt: Longident.parse("other"),
                  loc: conv_loc(loc),
                }),
              ),
            ),
          )
        );

  let match_expr =
    Ast_helper.(
      Exp.match(
        Ppx_config.native()
          ? [@metaloc conv_loc(loc)]
            [%expr value |> Yojson.Basic.Util.to_string]
          : [@metaloc conv_loc(loc)] [%expr (Obj.magic(value): string)],
        List.concat([enum_match_arms, [fallback_arm]]),
      )
    );

  %expr
  [%e match_expr];
};

let generate_fragment_parse_fun = (config, loc, name, arguments, definition) => {
  open Ast_helper;
  let ident =
    Ast_helper.Exp.ident({
      loc: loc |> Output_bucklescript_utils.conv_loc,
      txt: Longident.parse(name ++ ".verifyArgsAndParse"),
    });
  let variable_defs = get_variable_definitions(definition);
  let labeled_args =
    variable_defs
    |> List.filter(((name, _, _, _)) =>
         arguments |> List.exists(arg => arg == name)
       )
    |> List.map(((arg_name, type_, _span, type_span)) =>
         (
           Labelled(arg_name),
           Exp.variant(
             ~loc=
               config.map_loc(type_span) |> Output_bucklescript_utils.conv_loc,
             type_,
             None,
           ),
         )
       );

  Ast_helper.Exp.apply(
    ~loc=loc |> Output_bucklescript_utils.conv_loc,
    ident,
    List.append(
      labeled_args,
      [
        (
          Labelled("fragmentName"),
          Exp.variant(
            /// this is the location that happens in the error
            ~loc=loc |> Output_bucklescript_utils.conv_loc,
            name,
            None,
          ),
        ),
        (
          Nolabel,
          config.native
            ? Ast_helper.Exp.apply(
                Ast_helper.Exp.ident({
                  loc: Location.none,
                  txt: Longident.parse(name ++ ".unsafe_fromJson"),
                }),
                [(Nolabel, ident_from_string("value"))],
              )
            : ident_from_string(~loc=loc |> conv_loc, "value"),
        ),
      ],
    ),
  );
};

let generate_solo_fragment_spread_decoder =
    (config, loc, name, arguments, definition) => {
  generate_fragment_parse_fun(config, loc, name, arguments, definition);
};

let generate_error = (loc, message) => {
  let loc = Output_bucklescript_utils.conv_loc(loc);
  let ext = Ast_mapper.extension_of_error(Location.error(~loc, message));
  let%expr _value = value;
  %e
  Ast_helper.Exp.extension(~loc, ext);
};

let rec generate_nullable_decoder = (config, loc, inner, path, definition) =>
  config.native
    ? [@metaloc loc]
      (
        switch%expr (value) {
        | `Null => None
        | value =>
          Some([%e generate_parser(config, path, definition, inner)])
        }
      )
    : [@metaloc loc]
      (
        switch%expr (Js.toOption(value)) {
        | Some(value) =>
          Some([%e generate_parser(config, path, definition, inner)])
        | None => None
        }
      )
and generate_array_decoder = (config, loc, inner, path, definition) =>
  config.native
    ? [@metaloc loc]
      (
        switch%expr (value) {
        | `List(json_list) =>
          List.map(
            value => {
              %e
              generate_parser(config, path, definition, inner)
            },
            json_list,
          )
          |> Array.of_list
        | _ => [||]
        }
      )
    : [@metaloc loc]
      [%expr
        value
        |> Js.Array.map(value => {
             %e
             generate_parser(config, path, definition, inner)
           })
      ]
and generate_custom_decoder = (config, loc, ident, inner, path, definition) =>
  [@metaloc loc]
  {
    %expr
    [%e ident_from_string(ident ++ ".parse")](
      [%e generate_parser(config, path, definition, inner)],
    );
  }
and generate_object_decoder =
    (
      ~config,
      ~loc,
      ~name as _name,
      ~path,
      ~definition,
      ~existing_record,
      ~interface_fragments,
      fields,
    ) => {
  // whether we can use inline values, this compiles to better javascript
  // but we can't use this if we are constructing objects instead of records
  let do_obj_constructor_base = () => {
    open Ast_helper;
    let opaque = raw_opaque_object(interface_fragments, fields);
    let object_type = base_type_name("Raw." ++ generate_type_name(path));

    let get_value =
      fun
      | Fr_named_field({name: key, type_: inner}) =>
        config.native
          ? [@metaloc conv_loc(loc)]
            {
              let%expr value =
                Yojson.Basic.Util.member([%e const_str_expr(key)], value);

              %e
              generate_parser(config, [key, ...path], definition, inner);
            }
          : [@metaloc conv_loc(loc)]
            {
              let%expr value =
                switch%e (opaque) {
                | true =>
                  %expr
                  Obj.magic(
                    Js.Dict.unsafeGet(
                      Obj.magic(value),
                      [%e const_str_expr(key)],
                    ),
                  )

                | false =>
                  Ast_helper.Exp.field(
                    Exp.constraint_(ident_from_string("value"), object_type),
                    {
                      loc: Location.none,
                      Location.txt: Longident.parse(to_valid_ident(key)),
                    },
                  )
                };

              %e
              generate_parser(config, [key, ...path], definition, inner);
            }

      | Fr_fragment_spread({loc, name, arguments}) =>
        Ppx_config.native()
          ? generate_fragment_parse_fun(
              config,
              loc,
              name,
              arguments,
              definition,
            )
          : [@metaloc conv_loc(loc)]
            {
              let%expr value: [%t
                base_type_name(
                  ~loc=Output_bucklescript_utils.conv_loc(loc),
                  name ++ ".Raw.t",
                )
              ] =
                Obj.magic(value);
              %e
              generate_fragment_parse_fun(
                config,
                loc,
                name,
                arguments,
                definition,
              );
            };

    let get_record_contents_inline =
      fun
      | Fr_fragment_spread({key}) as field
      | Fr_named_field({name: key}) as field => (
          {txt: Longident.parse(to_valid_ident(key)), loc: conv_loc(loc)},
          get_value(field),
        );

    let record_fields = List.map(get_record_contents_inline, fields);

    let record_fields =
      switch (interface_fragments) {
      | None
      | Some((_, [])) => record_fields
      | Some((interface_name, fragments)) => [
          (
            {txt: Longident.Lident("fragment"), loc: Location.none},
            generate_poly_variant_interface_decoder(
              config,
              conv_loc(loc),
              interface_name,
              fragments,
              [interface_name, ...path],
              definition,
            ),
          ),
          ...record_fields,
        ]
      };

    Exp.record(record_fields, None);
  };

  [@metaloc loc]
  Ast_helper.(
    Exp.constraint_(
      do_obj_constructor_base(),
      base_type_name(
        switch (existing_record) {
        | None => generate_type_name(path)
        | Some(type_name) => type_name
        },
      ),
    )
  );
}
and generate_poly_variant_selection_set_decoder =
    (config, loc, name, fields, path, definition) => {
  let rec generator_loop =
    fun
    | [({item: field}: Result_structure.name, inner), ...next] => {
        let field_name = Compat.capitalize_ascii(field);
        let variant_decoder =
          Ast_helper.(
            Exp.variant(
              field_name,
              Some(
                generate_parser(config, [field, ...path], definition, inner),
              ),
            )
          );
        config.native
          ? [@metaloc loc]
            {
              let%expr temp =
                Yojson.Basic.Util.member([%e const_str_expr(field)], value);

              switch (temp) {
              | `Null =>
                let value = temp;
                %e
                variant_decoder;
              | _ =>
                %e
                generator_loop(next)
              };
            }
          : [@metaloc loc]
            {
              let%expr temp =
                Js.Dict.unsafeGet(
                  Obj.magic(value),
                  [%e const_str_expr(field)],
                );

              switch (Js.Json.decodeNull(temp)) {
              | None =>
                let value = temp;
                %e
                variant_decoder;
              | Some(_) =>
                %e
                generator_loop(next)
              };
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

  config.native
    ? [@metaloc loc]
      (
        switch%expr (value) {
        | `Null =>
          %e
          make_error_raiser(
            [%expr
              "Expected type "
              ++ [%e const_str_expr(name)]
              ++ " to be an object"
            ],
          )
        | value =>
          %e
          generator_loop(fields)
        }
      )
    : [@metaloc loc]
      (
        switch%expr (Js.Json.decodeObject(Obj.magic(value): Js.Json.t)) {
        | None =>
          %e
          make_error_raiser(
            [%expr
              "Expected type "
              ++ [%e const_str_expr(name)]
              ++ " to be an object"
            ],
          )
        | Some(value) =>
          %e
          generator_loop(fields)
        }
      );
}
and generate_poly_variant_interface_decoder =
    (config, loc, _name, fragments, path, definition) => {
  let fallback_case = {
    open Ast_helper;
    let name_pattern = Pat.any();

    Exp.variant("UnspecifiedFragment", Some([%expr typename]))
    |> Exp.case(name_pattern);
  };

  let fragment_cases =
    List.map(
      ((type_name, inner)) => {
        open Ast_helper;
        let name_pattern = const_str_pat(type_name);

        Exp.variant(
          type_name,
          Some(
            config.native
              ? generate_parser(
                  config,
                  [type_name, ...path],
                  definition,
                  inner,
                )
              : {
                let%expr value: [%t
                  base_type_name(
                    "Raw." ++ generate_type_name([type_name, ...path]),
                  )
                ] =
                  Obj.magic(value);
                %e
                generate_parser(
                  config,
                  [type_name, ...path],
                  definition,
                  inner,
                );
              },
          ),
        )
        |> Exp.case(name_pattern);
      },
      fragments,
    );
  let fallback_case = fallback_case;
  let typename_matcher =
    Ast_helper.(
      Exp.match(
        [%expr typename],
        List.concat([fragment_cases, [fallback_case]]),
      )
    );

  config.native
    ? [@metaloc loc]
      {
        let%expr typename: string =
          value
          |> Yojson.Basic.Util.member("__typename")
          |> Yojson.Basic.to_string;
        (
          [%e typename_matcher]: [%t base_type_name(generate_type_name(path))]
        );
      }
    : [@metaloc loc]
      {
        let%expr typename: string =
          Obj.magic(Js.Dict.unsafeGet(Obj.magic(value), "__typename"));
        (
          [%e typename_matcher]: [%t base_type_name(generate_type_name(path))]
        );
      };
}
and generate_poly_variant_union_decoder =
    (
      config,
      loc,
      _name,
      fragments,
      _exhaustive_flag,
      omit_future_value,
      path,
      definition,
    ) => {
  let fragment_cases =
    fragments
    |> List.map((({item: type_name}: Result_structure.name, inner)) => {
         Ast_helper.(
           Exp.case(
             const_str_pat(type_name),
             Exp.variant(
               type_name,
               Some(
                 config.native
                   ? generate_parser(
                       config,
                       [type_name, ...path],
                       definition,
                       inner,
                     )
                   : {
                     let%expr value: [%t
                       switch (inner) {
                       | Res_solo_fragment_spread({name}) =>
                         base_type_name(name ++ ".Raw.t")
                       | _ =>
                         base_type_name(
                           "Raw." ++ generate_type_name([type_name, ...path]),
                         )
                       }
                     ] =
                       Obj.magic(value);
                     %e
                     generate_parser(
                       config,
                       [type_name, ...path],
                       definition,
                       inner,
                     );
                   },
               ),
             ),
           )
         )
       });
  let fallback_case =
    omit_future_value
      ? Ast_helper.(
          Exp.case(
            Pat.any(),
            Exp.apply(
              Exp.ident(Location.mknoloc(Longident.parse("raise"))),
              [
                (
                  Nolabel,
                  Exp.construct(
                    Location.mknoloc(Longident.parse("Not_found")),
                    None,
                  ),
                ),
              ],
            ),
          )
        )
      : Ppx_config.native()
          ? Ast_helper.(
              Exp.case(
                Pat.any(),
                Exp.variant(
                  "FutureAddedValue",
                  Some(
                    [%expr
                      [%e
                        Exp.ident({
                          Location.txt: Longident.parse("value"),
                          loc: Location.none,
                        })
                      ]
                    ],
                  ),
                ),
              )
            )
          : Ast_helper.(
              Exp.case(
                Pat.any(),
                Exp.variant(
                  "FutureAddedValue",
                  Some(
                    [%expr
                      (
                        Obj.magic(
                          [%e
                            Exp.ident({
                              Location.txt: Longident.parse("value"),
                              loc: Location.none,
                            })
                          ],
                        ): Js.Json.t
                      )
                    ],
                  ),
                ),
              )
            );

  let typename_matcher =
    Ast_helper.(
      Exp.match(
        [%expr typename],
        List.concat([fragment_cases, [fallback_case]]),
      )
    );

  config.native
    ? [@metaloc loc]
      {
        let%expr typename: string =
          value
          |> Yojson.Basic.Util.member("__typename")
          |> Yojson.Basic.to_string;
        (
          [%e typename_matcher]: [%t base_type_name(generate_type_name(path))]
        );
      }
    : [@metaloc loc]
      {
        let%expr typename: string =
          Obj.magic(Js.Dict.unsafeGet(Obj.magic(value), "__typename"));
        (
          [%e typename_matcher]: [%t base_type_name(generate_type_name(path))]
        );
      };
}
and generate_parser = (config, path: list(string), definition) =>
  fun
  | Res_nullable({loc, inner}) =>
    generate_nullable_decoder(config, conv_loc(loc), inner, path, definition)
  | Res_array({loc, inner}) =>
    generate_array_decoder(config, conv_loc(loc), inner, path, definition)
  | Res_id({loc}) =>
    config.native
      ? [@metaloc conv_loc(loc)] [%expr Yojson.Basic.Util.to_string(value)]
      : [@metaloc conv_loc(loc)] [%expr value]
  | Res_string({loc}) =>
    config.native
      ? [@metaloc conv_loc(loc)] [%expr Yojson.Basic.Util.to_string(value)]
      : [@metaloc conv_loc(loc)] [%expr value]
  | Res_int({loc}) =>
    config.native
      ? [@metaloc conv_loc(loc)] [%expr Yojson.Basic.Util.to_int(value)]
      : [@metaloc conv_loc(loc)] [%expr value]
  | Res_float({loc}) =>
    config.native
      ? [@metaloc conv_loc(loc)] [%expr Yojson.Basic.Util.to_float(value)]
      : [@metaloc conv_loc(loc)] [%expr value]
  | Res_boolean({loc}) =>
    config.native
      ? [@metaloc conv_loc(loc)] [%expr Yojson.Basic.Util.to_bool(value)]
      : [@metaloc conv_loc(loc)] [%expr value]
  | Res_raw_scalar({loc}) => [@metaloc conv_loc(loc)] [%expr value]
  | Res_poly_enum({loc, enum_meta, omit_future_value}) =>
    generate_poly_enum_decoder(loc, enum_meta, omit_future_value)
  | Res_custom_decoder({loc, ident, inner}) =>
    generate_custom_decoder(
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
    generate_object_decoder(
      ~config,
      ~loc,
      ~name,
      ~path,
      ~definition,
      ~existing_record,
      ~interface_fragments,
      fields,
    )
  | Res_object({
      loc,
      name,
      fields,
      type_name: existing_record,
      interface_fragments,
    }) =>
    generate_object_decoder(
      ~config,
      ~loc,
      ~name,
      ~path,
      ~definition,
      ~existing_record,
      ~interface_fragments,
      fields,
    )
  | Res_poly_variant_union({
      loc,
      name,
      fragments,
      exhaustive,
      omit_future_value,
    }) =>
    generate_poly_variant_union_decoder(
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
    generate_poly_variant_selection_set_decoder(
      config,
      conv_loc(loc),
      name,
      fields,
      path,
      definition,
    )

  | Res_poly_variant_interface({loc, name, fragments}) =>
    generate_poly_variant_interface_decoder(
      config,
      conv_loc(loc),
      name,
      fragments,
      [name, ...path],
      definition,
    )
  | Res_solo_fragment_spread({loc, name, arguments}) =>
    generate_solo_fragment_spread_decoder(
      config,
      loc,
      name,
      arguments,
      definition,
    )
  | Res_error({loc, message}) => generate_error(loc, message);

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

let type_name_to_words = type_name => {
  let str = ref("");
  type_name
  |> String.iter(
       fun
       | '!' => str := str^ ++ "_NonNull"
       | ']' => str := str^ ++ "_OfList"
       | c => str := str^ ++ String.make(1, c),
     );
  str^;
};

let get_variable_definitions = (definition: Graphql_ast.definition) => {
  switch (definition) {
  | Fragment({item: {fg_directives: directives}}) =>
    Result_decoder.getFragmentArgumentDefinitions(directives)
    |> List.map(((name, type_, span, type_span)) =>
         (name, type_name_to_words(type_), span, type_span)
       )
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
               (
                 name,
                 Graphql_printer.print_type(type_ref) |> type_name_to_words,
                 span,
                 type_span,
               ),
               ...acc,
             ],
           [],
         )
    )
  | _ => []
  };
};

let make_error_raiser = message =>
  if (Ppx_config.verbose_error_handling()) {
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
        [@metaloc conv_loc(loc)] [%expr Obj.magic(value: string)],
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
      loc: loc |> conv_loc,
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
        (Nolabel, ident_from_string(~loc=loc |> conv_loc, "value")),
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
  [@metaloc loc]
  (
    switch%expr (Js.toOption(value)) {
    | Some(value) =>
      Some([%e generate_parser(config, path, definition, inner)])
    | None => None
    }
  )
and generate_array_decoder = (config, loc, inner, path, definition) =>
  [@metaloc loc]
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
      config,
      loc,
      _name,
      fields,
      path,
      definition,
      existing_record,
      force_record,
    ) => {
  // whether we can use inline values, this compiles to better javascript
  // but we can't use this if we are constructing objects instead of records
  let inline_values = config.records;
  let do_obj_constructor_base = (is_object, wrap) => {
    open Ast_helper;
    let opaque =
      fields
      |> List.exists(
           fun
           | Fr_fragment_spread(_) => true
           | _ => false,
         );
    let object_type = base_type_name("Raw." ++ generate_type_name(path));

    let get_value =
      fun
      | Fr_named_field({name: key, type_: inner}) =>
        [@metaloc conv_loc(loc)]
        {
          let%expr value =
            switch%e (opaque, is_object) {
            | (true, _) =>
              %expr
              Obj.magic(
                Js.Dict.unsafeGet(
                  Obj.magic(value),
                  [%e const_str_expr(key)],
                ),
              )

            | (_, true) =>
              %expr
              value##[%e ident_from_string(to_valid_ident(key))]
            | (_, false) =>
              %expr
              [%e
                Ast_helper.Exp.field(
                  Exp.constraint_(ident_from_string("value"), object_type),
                  {
                    loc: Location.none,
                    Location.txt: Longident.parse(to_valid_ident(key)),
                  },
                )
              ]
            };

          %e
          generate_parser(config, [key, ...path], definition, inner);
        }

      | Fr_fragment_spread(_key, loc, name, _, arguments) =>
        [@metaloc conv_loc(loc)]
        {
          let%expr value: [%t base_type_name(name ++ ".Raw.t")] =
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

    let get_record_contents =
      fun
      | Fr_fragment_spread(key, _, _, _, _)
      | Fr_named_field({name: key}) => (
          {txt: Longident.parse(to_valid_ident(key)), loc: conv_loc(loc)},
          ident_from_string(to_valid_ident(key)),
        );

    let get_record_contents_inline =
      fun
      | Fr_fragment_spread(key, _, _, _, _) as field
      | Fr_named_field({name: key}) as field => (
          {txt: Longident.parse(to_valid_ident(key)), loc: conv_loc(loc)},
          get_value(field),
        );

    let record =
      Exp.record(
        List.map(
          inline_values ? get_record_contents_inline : get_record_contents,
          fields,
        ),
        None,
      );

    let record = wrap ? record_to_object(loc, record) : record;

    inline_values
      ? record
      : {
        let bindings =
          fields
          |> List.map(
               fun
               | Fr_named_field({name}) as field => (name, field)
               | Fr_fragment_spread(key, _, _, _, _) as field => (
                   key,
                   field,
                 ),
             )
          |> List.map(((key, field)) => {
               Vb.mk(
                 Pat.var({txt: to_valid_ident(key), loc: conv_loc(loc)}),
                 get_value(field),
               )
             })
          |> List.rev;
        Exp.let_(Nonrecursive, bindings, record);
      };
  };

  let do_obj_constructor = () =>
    [@metaloc loc]
    {
      do_obj_constructor_base(true, true);
    };

  let do_obj_constructor_records = () =>
    [@metaloc loc]
    {
      Ast_helper.(
        Exp.constraint_(
          do_obj_constructor_base(!config.records, false),
          base_type_name(
            switch (existing_record) {
            | None => generate_type_name(path)
            | Some(type_name) => type_name
            },
          ),
        )
      );
    };

  config.records || existing_record != None || force_record
    ? do_obj_constructor_records() : do_obj_constructor();
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
        [@metaloc loc]
        {
          let%expr temp =
            Js.Dict.unsafeGet(Obj.magic(value), [%e const_str_expr(field)]);

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

  [@metaloc loc]
  (
    switch%expr (Js.Json.decodeObject(Obj.magic(value): Js.Json.t)) {
    | None =>
      %e
      make_error_raiser(
        [%expr
          "Expected type " ++ [%e const_str_expr(name)] ++ " to be an object"
        ],
      )
    | Some(value) =>
      %e
      generator_loop(fields)
    }
  );
}
and generate_poly_variant_interface_decoder =
    (config, loc, _name, base, fragments, path, definition) => {
  let map_fallback_case = ((type_name, inner)) => {
    open Ast_helper;
    let name_pattern = Pat.any();

    Exp.variant(
      type_name,
      Some(
        generate_parser(config, [type_name, ...path], definition, inner),
      ),
    )
    |> Exp.case(name_pattern);
  };

  let map_case = ((type_name, _inner)) => {
    open Ast_helper;
    let name_pattern = const_str_pat(type_name);

    Exp.variant(
      type_name,
      Some(
        {
          let%expr value: [%t
            base_type_name(
              "Raw." ++ generate_type_name([type_name, ...path]),
            )
          ] =
            Obj.magic(value);
          generate_parser(config, [type_name, ...path], definition, inner);
        },
      ),
    )
    |> Exp.case(name_pattern);
  };

  let fragment_cases = List.map(map_case, fragments);
  let fallback_case = map_fallback_case(base);
  let typename_matcher =
    Ast_helper.(
      Exp.match(
        [%expr typename],
        List.concat([fragment_cases, [fallback_case]]),
      )
    );

  [@metaloc loc]
  {
    let%expr typename: string =
      Obj.magic(Js.Dict.unsafeGet(Obj.magic(value), "__typename"));
    ([%e typename_matcher]: [%t base_type_name(generate_type_name(path))]);
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
                 {
                   let%expr value: [%t
                     switch (inner) {
                     | Res_solo_fragment_spread(_, name, _) =>
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

  [@metaloc loc]
  {
    let%expr typename: string =
      Obj.magic(Js.Dict.unsafeGet(Obj.magic(value), "__typename"));
    ([%e typename_matcher]: [%t base_type_name(generate_type_name(path))]);
  };
}
and generate_parser = (config, path: list(string), definition) =>
  fun
  | Res_nullable(loc, inner) =>
    generate_nullable_decoder(config, conv_loc(loc), inner, path, definition)
  | Res_array(loc, inner) =>
    generate_array_decoder(config, conv_loc(loc), inner, path, definition)
  | Res_id(loc) => raw_value(conv_loc(loc))
  | Res_string(loc) => raw_value(conv_loc(loc))
  | Res_int(loc) => raw_value(conv_loc(loc))
  | Res_float(loc) => raw_value(conv_loc(loc))
  | Res_boolean(loc) => raw_value(conv_loc(loc))
  | Res_raw_scalar(loc) => raw_value(conv_loc(loc))
  | Res_poly_enum(loc, enum_meta, omit_future_value) =>
    generate_poly_enum_decoder(loc, enum_meta, omit_future_value)
  | Res_custom_decoder(loc, ident, inner) =>
    generate_custom_decoder(
      config,
      conv_loc(loc),
      ident,
      inner,
      path,
      definition,
    )
  | Res_record(loc, name, fields, existing_record) =>
    generate_object_decoder(
      config,
      loc,
      name,
      fields,
      path,
      definition,
      existing_record,
      true,
    )
  | Res_object(loc, name, fields, existing_record) =>
    generate_object_decoder(
      config,
      loc,
      name,
      fields,
      path,
      definition,
      existing_record,
      false,
    )
  | Res_poly_variant_union(
      loc,
      name,
      fragments,
      exhaustive,
      omit_future_value,
    ) =>
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
  | Res_poly_variant_selection_set(loc, name, fields) =>
    generate_poly_variant_selection_set_decoder(
      config,
      conv_loc(loc),
      name,
      fields,
      path,
      definition,
    )

  | Res_poly_variant_interface(loc, name, base, fragments) =>
    generate_poly_variant_interface_decoder(
      config,
      conv_loc(loc),
      name,
      base,
      fragments,
      [name, ...path],
      definition,
    )
  | Res_solo_fragment_spread(loc, name, arguments) =>
    generate_solo_fragment_spread_decoder(
      config,
      loc,
      name,
      arguments,
      definition,
    )
  | Res_error(loc, message) => generate_error(loc, message);

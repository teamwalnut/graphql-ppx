open Migrate_parsetree;
open Graphql_ppx_base;
open Result_structure;
open Extract_type_definitions;
open Source_pos;
open Output_bucklescript_utils;

open Ast_408;
open Parsetree;

// duplicate of ouput_bucklescript_decoder
let make_error_raiser = message =>
  if (Ppx_config.verbose_error_handling()) {
    %expr
    Js.Exn.raiseError("graphql_ppx: " ++ [%e message]);
  } else {
    %expr
    Js.Exn.raiseError("Unexpected GraphQL query response");
  };
// duplicate of ouput_bucklescript_decoder
let const_str_expr = s => Ast_helper.(Exp.constant(Pconst_string(s, None)));

let base_type = (~inner=[], ~loc=?, name) => {
  Ast_helper.Typ.constr(
    {
      Location.txt: Longident.parse(name),
      loc:
        switch (loc) {
        | None => Location.none
        | Some(loc) => loc
        },
    },
    inner,
  );
};

// generate the type definition, including nullables, arrays etc.
let rec generate_type = (~atLoc=?, config, path, raw) =>
  fun
  | Res_string(loc) => base_type(~loc=?atLoc, "string")
  | Res_nullable(loc, inner) =>
    if (raw) {
      base_type(
        ~inner=[generate_type(config, path, raw, inner)],
        "Js.Nullable.t",
      );
    } else {
      base_type(
        ~loc=?atLoc,
        ~inner=[generate_type(config, path, raw, inner)],
        "option",
      );
    }
  | Res_array(loc, inner) =>
    base_type(
      ~loc=?atLoc,
      ~inner=[generate_type(config, path, raw, inner)],
      "array",
    )
  | Res_custom_decoder(loc, module_name, inner) =>
    if (raw) {
      generate_type(config, path, raw, inner);
    } else {
      base_type(~loc=?atLoc, module_name ++ ".t");
    }
  | Res_id(loc) => base_type(~loc=?atLoc, "string")
  | Res_int(loc) => {
      base_type(~loc=?atLoc, "int");
    }
  | Res_float(loc) => base_type(~loc=?atLoc, "float")
  | Res_boolean(loc) => base_type(~loc=?atLoc, "bool")
  | Res_raw_scalar(loc) => base_type(~loc=?atLoc, "Js.Json.t")
  | Res_object(loc, name, _fields, type_name)
  | Res_record(loc, name, _fields, type_name) =>
    switch (type_name, raw) {
    | (Some(type_name), false) => base_type(~loc=?atLoc, type_name)
    | (_, _) => base_type(~loc=?atLoc, generate_type_name(path))
    }
  | Res_poly_variant_selection_set(loc, name, _)
  | Res_poly_variant_union(loc, name, _, _)
  | Res_poly_variant_interface(loc, name, _, _) => {
      base_type(~loc=?atLoc, generate_type_name(path));
    }
  | Res_solo_fragment_spread(loc, module_name, _arguments) =>
    if (raw) {
      base_type(module_name ++ ".Raw.t");
    } else {
      base_type(~loc=?atLoc, module_name ++ ".t");
    }
  | Res_error(loc, error) =>
    raise(Location.Error(Location.error(~loc=conv_loc(loc), error)))
  | Res_poly_enum(loc, enum_meta) =>
    base_type(~loc=?atLoc, generate_type_name(path));

let wrap_type_declaration = (~manifest=?, inner, loc, path) => {
  Ast_helper.Type.mk(
    ~kind=inner,
    ~manifest?,
    {loc: Location.none, txt: generate_type_name(path)},
  );
};

let generate_opaque = (path, loc) => {
  Ast_helper.Type.mk({loc: Location.none, txt: generate_type_name(path)});
};

let raw_opaque_object = fields => {
  fields
  |> List.exists(
       fun
       | Fragment(_) => true
       | _ => false,
     );
};

let already_has__typename = fields =>
  List.exists(
    fun
    | Field({path: ["__typename", ..._]}) => true
    | _ => false,
    fields,
  );

let generate_record_type = (config, fields, obj_path, raw, loc, is_variant) => {
  let record_fields =
    fields
    |> List.fold_left(
         acc =>
           fun
           | Fragment({key, module_name, type_name}) => [
               Ast_helper.Type.field(
                 {Location.txt: key, loc: Location.none},
                 Ast_helper.Typ.constr(
                   {
                     Location.txt:
                       Longident.parse(
                         module_name
                         ++ ".t"
                         ++ (
                           switch (type_name) {
                           | None => ""
                           | Some(type_name) => "_" ++ type_name
                           }
                         ),
                       ),
                     loc: Location.none,
                   },
                   [],
                 ),
               ),
               ...acc,
             ]
           | Field({path: [name, ...path], type_, loc, loc_key}) => {
               [
                 Ast_helper.Type.field(
                   {Location.txt: to_valid_ident(name), loc: Location.none},
                   generate_type(
                     ~atLoc=?raw ? None : Some(conv_loc(loc_key)),
                     config,
                     [name, ...path],
                     raw,
                     type_,
                   ),
                 ),
                 ...acc,
               ];
             }
           | Field({path: [], loc}) =>
             // I don't think this should ever happen but we need to
             // cover this case, perhaps we can constrain the type
             raise(
               Location.Error(
                 Location.error(~loc=loc |> conv_loc, "No path"),
               ),
             ),
         [],
       )
    |> List.rev;

  let record_fields =
    // if this is a variant in the parsed type and __typename is not explicitly
    // requested, still add it (because the printer is added)
    if (raw && is_variant && !already_has__typename(fields)) {
      [
        Ast_helper.Type.field(
          {Location.txt: "__typename", loc: Location.none},
          base_type_name("string"),
        ),
        ...record_fields,
      ];
    } else {
      record_fields;
    };

  raw && raw_opaque_object(fields)
    ? generate_opaque(obj_path, loc)
    : wrap_type_declaration(Ptype_record(record_fields), loc, obj_path);
};

let generate_variant_selection = (config, fields, path, loc, raw) =>
  if (raw) {
    generate_opaque(path, loc);
  } else {
    wrap_type_declaration(
      Ptype_abstract,
      ~manifest=
        Ast_helper.(
          Typ.variant(
            fields
            |> List.map(((name: Result_structure.name, res)) =>
                 {
                   prf_desc:
                     Rtag(
                       {
                         txt: Compat.capitalize_ascii(name.item),
                         loc: Location.none,
                       },
                       false,
                       [
                         generate_type(
                           ~atLoc=
                             conv_loc(
                               config.Generator_utils.map_loc(name.span),
                             ),
                           config,
                           [name.item, ...path],
                           raw,
                           res,
                         ),
                       ],
                     ),
                   prf_loc: Location.none,
                   prf_attributes: [],
                 }
               ),
            Closed,
            None,
          )
        ),
      loc,
      path,
    );
  };

let generate_variant_union =
    (
      config,
      fields: list((Result_structure.name, Result_structure.t)),
      path,
      loc,
      raw,
    ) =>
  if (raw) {
    generate_opaque(path, loc);
  } else {
    let fallback_case_ty = [
      {
        prf_desc:
          Rtag(
            {txt: "FutureAddedValue", loc: conv_loc(loc)},
            false,
            [base_type("Js.Json.t")],
          ),
        prf_loc: conv_loc(loc),
        prf_attributes: [],
      },
    ];

    let fragment_case_tys =
      fields
      |> List.map(((name, res)) =>
           {
             prf_desc:
               Rtag(
                 {txt: name.item, loc: conv_loc(loc)},
                 false,
                 [
                   generate_type(
                     ~atLoc=
                       conv_loc(config.Generator_utils.map_loc(name.span)),
                     config,
                     [name.item, ...path],
                     raw,
                     res,
                   ),
                 ],
               ),
             prf_loc: conv_loc(loc),
             prf_attributes: [],
           }
         );

    wrap_type_declaration(
      Ptype_abstract,
      ~manifest=
        Ast_helper.(
          Typ.variant(
            List.concat([fallback_case_ty, fragment_case_tys]),
            Closed,
            None,
          )
        ),
      loc,
      path,
    );
  };

let generate_variant_interface = (config, fields, base, path, loc, raw) =>
  if (raw) {
    generate_opaque(path, loc);
  } else {
    let map_case_ty = ((name, res)) => {
      prf_desc:
        Rtag(
          {txt: name, loc: conv_loc(loc)},
          false,
          [generate_type(config, [name, ...path], raw, res)],
        ),
      prf_loc: conv_loc(loc),
      prf_attributes: [],
    };

    let fallback_case_ty = map_case_ty(base);
    let fragment_case_tys = fields |> List.map(map_case_ty);

    wrap_type_declaration(
      Ptype_abstract,
      ~manifest=
        Ast_helper.(
          Typ.variant([fallback_case_ty, ...fragment_case_tys], Closed, None)
        ),
      loc,
      path,
    );
  };

let generate_enum = (config, fields, path, loc, raw) =>
  wrap_type_declaration(
    Ptype_abstract,
    ~manifest=
      if (raw) {
        base_type("string");
      } else {
        Graphql_ppx_base__.Schema.(
          [@metaloc conv_loc(loc)]
          Ast_helper.(
            Typ.variant(
              [
                {
                  prf_desc:
                    Rtag(
                      {txt: "FutureAddedValue", loc: conv_loc(loc)},
                      false,
                      [base_type("string")],
                    ),
                  prf_loc: conv_loc(loc),
                  prf_attributes: [],
                },
                ...fields
                   |> List.map(field =>
                        {
                          prf_desc:
                            Rtag(
                              {
                                txt: to_valid_ident(field),
                                loc: conv_loc(loc),
                              },
                              true,
                              [],
                            ),
                          prf_loc: conv_loc(loc),
                          prf_attributes: [],
                        }
                      ),
              ],
              Closed,
              None,
            )
          )
        );
      },
    loc,
    path,
  );

let generate_object_type = (config, fields, obj_path, raw, loc, is_variant) => {
  let object_fields =
    fields
    |> List.fold_left(
         acc =>
           fun
           | Fragment({key, module_name, type_name}) => [
               {
                 pof_desc:
                   Otag(
                     {txt: key, loc: Location.none},
                     Ast_helper.Typ.constr(
                       {
                         Location.txt:
                           Longident.parse(
                             module_name
                             ++ ".t"
                             ++ (
                               switch (type_name) {
                               | None => ""
                               | Some(type_name) => "_" ++ type_name
                               }
                             ),
                           ),
                         loc: Location.none,
                       },
                       [],
                     ),
                   ),
                 pof_loc: Location.none,
                 pof_attributes: [],
               },
               ...acc,
             ]

           | Field({path: [name, ...path], loc_key, type_}) => [
               {
                 pof_desc:
                   Otag(
                     {txt: to_valid_ident(name), loc: Location.none},
                     generate_type(
                       ~atLoc=?raw ? None : Some(conv_loc(loc_key)),
                       config,
                       [name, ...path],
                       raw,
                       type_,
                     ),
                   ),
                 pof_loc: Location.none,
                 pof_attributes: [],
               },
               ...acc,
             ]
           | Field({path: [], loc}) =>
             // I don't think this should ever happen but we need to
             // cover this case, perhaps we can constrain the type
             raise(
               Location.Error(
                 Location.error(~loc=loc |> conv_loc, "No path"),
               ),
             ),
         [],
       )
    |> List.rev;

  let object_fields =
    // if this is a variant in the parsed type and __typename is not explicitly
    // requested, still add it (because the printer is added)
    if (raw && is_variant && !already_has__typename(fields)) {
      [
        {
          pof_desc:
            Otag(
              {txt: to_valid_ident("__typename"), loc: Location.none},
              base_type_name("string"),
            ),
          pof_loc: Location.none,
          pof_attributes: [],
        },
        ...object_fields,
      ];
    } else {
      object_fields;
    };

  raw && raw_opaque_object(fields)
    ? generate_opaque(obj_path, loc)
    : wrap_type_declaration(
        ~manifest=
          Ast_helper.(
            Typ.constr(
              {Location.txt: Longident.parse("Js.t"), loc: Location.none},
              [Ast_helper.Typ.object_(object_fields, Closed)],
            )
          ),
        Ptype_abstract,
        loc,
        obj_path,
      );
};

let generate_graphql_object =
    (
      config: Generator_utils.output_config,
      fields,
      obj_path,
      force_record,
      raw,
      loc,
      is_variant,
    ) => {
  config.records || force_record
    ? generate_record_type(config, fields, obj_path, raw, loc, is_variant)
    : generate_object_type(config, fields, obj_path, raw, loc, is_variant);
};

// generate all the types necessary types that we later refer to by name.
let generate_types =
    (config: Generator_utils.output_config, res, raw, fragment_name) => {
  let types =
    extract(~path=[], ~raw, res)
    |> List.map(
         fun
         | Object({fields, path: obj_path, force_record, loc, variant_parent}) =>
           generate_graphql_object(
             config,
             fields,
             obj_path,
             force_record,
             raw,
             loc,
             variant_parent,
           )
         | VariantSelection({loc, path, fields}) =>
           generate_variant_selection(config, fields, path, loc, raw)
         | VariantUnion({loc, path, fields}) =>
           generate_variant_union(config, fields, path, loc, raw)
         | VariantInterface({loc, path, base, fields}) =>
           generate_variant_interface(config, fields, base, path, loc, raw)
         | Enum({loc, path, fields}) =>
           generate_enum(config, fields, path, loc, raw),
       )
    |> List.rev;

  let types =
    types |> List.map(type_ => Ast_helper.Str.type_(Recursive, [type_]));
  switch (fragment_name) {
  | Some((fragment_name, fragment_name_loc)) =>
    List.append(
      types,
      [
        Ast_helper.(
          Str.type_(
            Nonrecursive,
            [
              Type.mk(
                ~manifest=
                  Typ.constr(
                    raw
                      ? Location.mknoloc(Longident.Lident("t"))
                      : Location.mkloc(
                          Longident.Lident("t"),
                          conv_loc(config.map_loc(fragment_name_loc)),
                        ),
                    [],
                  ),
                Location.mknoloc("t_" ++ fragment_name),
              ),
            ],
          )
        ),
      ],
    )
  | None => types
  };
};

let rec generate_arg_type = (raw, loc as originalLoc) => {
  let loc = raw ? None : Some(conv_loc(originalLoc));
  fun
  | Type(Scalar({sm_name: "ID"}))
  | Type(Scalar({sm_name: "String"})) => base_type(~loc?, "string")
  | Type(Scalar({sm_name: "Int"})) => base_type(~loc?, "int")
  | Type(Scalar({sm_name: "Float"})) => base_type(~loc?, "float")
  | Type(Scalar({sm_name: "Boolean"})) => base_type(~loc?, "bool")
  | Type(Scalar({sm_name: _})) => base_type(~loc?, "Js.Json.t")
  | Type(Enum(enum_meta)) =>
    if (raw) {
      base_type("string");
    } else {
      Graphql_ppx_base__.Schema.(
        Ast_helper.(
          Typ.variant(
            ~loc?,
            enum_meta.em_values
            |> List.map(({evm_name, _}) =>
                 {
                   prf_desc:
                     Rtag({txt: evm_name, loc: Location.none}, true, []),
                   prf_loc: Location.none,
                   prf_attributes: [],
                 }
               ),
            Closed,
            None,
          )
        )
      );
    }
  | Type(InputObject({iom_name})) =>
    base_type(~loc?, generate_type_name(~prefix="t_variables", [iom_name]))
  | Type(Object(_)) =>
    raise(
      Location.Error(
        Location.error(
          ~loc=originalLoc |> conv_loc,
          "Object not allowed in args",
        ),
      ),
    )
  | Type(Union(_)) =>
    raise(
      Location.Error(
        Location.error(
          ~loc=originalLoc |> conv_loc,
          "Union not allowed in args",
        ),
      ),
    )
  | Type(Interface(_)) =>
    raise(
      Location.Error(
        Location.error(
          ~loc=originalLoc |> conv_loc,
          "Interface not allowed in args",
        ),
      ),
    )
  | Nullable(inner) =>
    base_type(
      ~loc?,
      ~inner=[
        generate_arg_type(raw, conv_loc_from_ast(Location.none), inner),
      ],
      raw ? "Js.Nullable.t" : "option",
    )
  | List(inner) =>
    base_type(
      ~loc?,
      ~inner=[
        generate_arg_type(raw, conv_loc_from_ast(Location.none), inner),
      ],
      "array",
    )
  | TypeNotFound(name) =>
    raise(
      Location.Error(
        Location.error(
          ~loc=originalLoc |> conv_loc,
          "Type " ++ name ++ " not found!",
        ),
      ),
    );
};

let generate_record_input_object = (raw, input_obj_name, fields) => {
  Ast_helper.Type.mk(
    ~kind=
      Ptype_record(
        fields
        |> List.map(
             fun
             | InputField({name, type_, loc, loc_type}) => {
                 Ast_helper.Type.field(
                   {Location.txt: name, loc: Location.none},
                   generate_arg_type(
                     raw,
                     {
                       ...loc,
                       loc_ghost:
                         switch (input_obj_name) {
                         | None => false
                         | Some(_) => true
                         },
                     },
                     type_,
                   ),
                 );
               },
           ),
      ),
    {
      loc: Location.none,
      txt:
        generate_type_name(
          ~prefix="t_variables",
          switch (input_obj_name) {
          | None => []
          | Some(name) => [name]
          },
        ),
    },
  );
};

let generate_object_input_object = (raw, input_obj_name, fields) => {
  Ast_helper.(
    Type.mk(
      ~kind=Ptype_abstract,
      ~manifest=
        Typ.constr(
          {Location.txt: Longident.parse("Js.t"), loc: Location.none},
          [
            Ast_helper.Typ.object_(
              fields
              |> List.map((InputField({name, type_, loc})) =>
                   {
                     pof_desc:
                       Otag(
                         {txt: name, loc: Location.none},
                         generate_arg_type(raw, loc, type_),
                       ),
                     pof_loc: Location.none,
                     pof_attributes: [],
                   }
                 ),
              Closed,
            ),
          ],
        ),
      {
        loc: Location.none,
        txt:
          generate_type_name(
            ~prefix="t_variables",
            switch (input_obj_name) {
            | None => []
            | Some(name) => [name]
            },
          ),
      },
    )
  );
};

let generate_input_object =
    (raw, config: Generator_utils.output_config, input_obj_name, fields) => {
  config.records
    ? generate_record_input_object(raw, input_obj_name, fields)
    : generate_object_input_object(raw, input_obj_name, fields);
};

let generate_arg_types = (raw, config, variable_defs) => {
  let input_objects = extract_args(config, variable_defs);

  [
    input_objects
    |> List.map((InputObject({name, fields})) => {
         generate_input_object(raw, config, name, fields)
       })
    |> Ast_helper.Str.type_(Recursive),
    ...input_objects
       |> List.fold_left(
            (p, InputObject({name, fields})) => {
              switch (name) {
              | None =>
                List.append(
                  p,
                  fields
                  |> List.fold_left(
                       (p, field) => {
                         List.append(
                           p,
                           Output_bucklescript_docstrings.for_input_constraint(
                             config,
                             field,
                           ),
                         )
                       },
                       [],
                     ),
                )
              | Some(_) => p
              }
            },
            [],
          ),
  ];
};

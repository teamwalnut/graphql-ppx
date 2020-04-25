open Migrate_parsetree;
open Graphql_ppx_base;
open Result_structure;
open Extract_type_definitions;
open Source_pos;
open Output_bucklescript_utils;

open Ast_408;
open Parsetree;

let to_valid_ident = ident =>
  if (ident.[0] >= '0' && ident.[0] <= '9') {
    "_" ++ ident;
  } else {
    [
      "and",
      "as",
      "asr",
      "assert",
      "begin",
      "class",
      "constraint",
      "do",
      "done",
      "downto",
      "else",
      "end",
      "esfun",
      "exception",
      "external",
      "false",
      "for",
      "fun",
      "function",
      "functor",
      "if",
      "in",
      "include",
      "inherit",
      "initializer",
      "land",
      "lazy",
      "let",
      "lor",
      "lsl",
      "lsr",
      "lxor",
      "match",
      "method",
      "mod",
      "module",
      "mutable",
      "new",
      "nonrec",
      "object",
      "of",
      "open",
      "open!",
      "or",
      "pri",
      "private",
      "pub",
      "public",
      "rec",
      "sig",
      "struct",
      "switch",
      "then",
      "to",
      "true",
      "try",
      "type",
      "val",
      "virtual",
      "when",
      "while",
      "with",
    ]
    |> List.exists(reserved_word => ident == reserved_word)
      ? ident ++ "_" : ident;
  };

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
let rec generate_type = (config, path, raw) =>
  fun
  | Res_string(loc) => base_type(~loc=conv_loc(loc), "string")
  | Res_nullable(loc, inner) =>
    if (raw) {
      base_type(
        ~loc=conv_loc(loc),
        ~inner=[generate_type(config, path, raw, inner)],
        "Js.Nullable.t",
      );
    } else {
      base_type(
        ~loc=conv_loc(loc),
        ~inner=[generate_type(config, path, raw, inner)],
        "option",
      );
    }
  | Res_array(loc, inner) =>
    base_type(
      ~loc=conv_loc(loc),
      ~inner=[generate_type(config, path, raw, inner)],
      "array",
    )
  | Res_custom_decoder(loc, module_name, inner) =>
    if (raw) {
      generate_type(config, path, raw, inner);
    } else {
      base_type(~loc=conv_loc(loc), module_name ++ ".t");
    }
  | Res_id(loc) => base_type(~loc=conv_loc(loc), "string")
  | Res_int(loc) => base_type(~loc=conv_loc(loc), "int")
  | Res_float(loc) => base_type(~loc=conv_loc(loc), "float")
  | Res_boolean(loc) => base_type(~loc=conv_loc(loc), "bool")
  | Res_raw_scalar(loc) => base_type(~loc=conv_loc(loc), "Js.Json.t")
  | Res_object(loc, name, _fields, Some(type_name))
  | Res_record(loc, name, _fields, Some(type_name)) =>
    base_type(~loc=conv_loc(loc), type_name)
  | Res_object(loc, name, _fields, None)
  | Res_record(loc, name, _fields, None) =>
    base_type(~loc=conv_loc(loc), generate_type_name(path))
  | Res_poly_variant_selection_set(loc, name, _)
  | Res_poly_variant_union(loc, name, _, _)
  | Res_poly_variant_interface(loc, name, _, _) =>
    base_type(~loc=conv_loc(loc), generate_type_name(path))
  | Res_solo_fragment_spread(loc, module_name, _arguments) =>
    if (raw) {
      base_type(module_name ++ ".Raw.t");
    } else {
      base_type(module_name ++ ".t");
    }
  | Res_error(loc, error) =>
    raise(Location.Error(Location.error(~loc=conv_loc(loc), error)))
  | Res_poly_enum(loc, enum_meta) =>
    base_type(~loc=conv_loc(loc), generate_type_name(path));

let wrap_type_declaration = (~manifest=?, inner, loc, path) => {
  Ast_helper.Type.mk(
    ~kind=inner,
    ~manifest?,
    {loc: conv_loc(loc), txt: generate_type_name(path)},
  );
};

let generate_opaque = (path, loc) => {
  Ast_helper.Type.mk({loc: conv_loc(loc), txt: generate_type_name(path)});
};

let raw_opaque_object = fields => {
  fields
  |> List.exists(
       fun
       | Fragment(_) => true
       | _ => false,
     );
};

let generate_record_type = (config, fields, obj_path, raw, loc) => {
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
           | Field({path: [name, ...path], type_}) => [
               Ast_helper.Type.field(
                 {Location.txt: to_valid_ident(name), loc: Location.none},
                 generate_type(config, [name, ...path], raw, type_),
               ),
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
            |> List.map(((name, _)) =>
                 {
                   prf_desc:
                     Rtag(
                       {
                         txt: Compat.capitalize_ascii(name),
                         loc: conv_loc(loc),
                       },
                       false,
                       [
                         {
                           ptyp_desc: Ptyp_any,
                           ptyp_attributes: [],
                           ptyp_loc_stack: [],
                           ptyp_loc: Location.none,
                         },
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

let generate_variant_union = (config, fields, path, loc, raw) =>
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
                 {txt: name, loc: conv_loc(loc)},
                 false,
                 [generate_type(config, [name, ...path], raw, res)],
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

let generate_object_type = (config, fields, obj_path, raw, loc) => {
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

           | Field({path: [name, ...path], type_}) => [
               {
                 pof_desc:
                   Otag(
                     {txt: to_valid_ident(name), loc: Location.none},
                     generate_type(config, [name, ...path], raw, type_),
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
    ) => {
  config.records || force_record
    ? generate_record_type(config, fields, obj_path, raw, loc)
    : generate_object_type(config, fields, obj_path, raw, loc);
};

// generate all the types necessary types that we later refer to by name.
let generate_types =
    (config: Generator_utils.output_config, res, raw, fragment_name) => {
  let types =
    extract([], res)
    |> List.map(
         fun
         | Object({fields, path: obj_path, force_record, loc}) =>
           generate_graphql_object(
             config,
             fields,
             obj_path,
             force_record,
             raw,
             loc,
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
  | Some(fragment_name) =>
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
                    {loc: Location.none, txt: Longident.Lident("t")},
                    [],
                  ),
                {loc: Location.none, txt: "t_" ++ fragment_name},
              ),
            ],
          )
        ),
      ],
    )
  | None => types
  };
};

let rec generate_arg_type = loc =>
  fun
  | Type(Scalar({sm_name: "ID"}))
  | Type(Scalar({sm_name: "String"})) => base_type("string")
  | Type(Scalar({sm_name: "Int"})) => base_type("int")
  | Type(Scalar({sm_name: "Float"})) => base_type("float")
  | Type(Scalar({sm_name: "Boolean"})) => base_type("bool")
  | Type(Scalar({sm_name: _})) => base_type("Js.Json.t")
  | Type(Enum(enum_meta)) =>
    Graphql_ppx_base__.Schema.(
      Ast_helper.(
        Typ.variant(
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
    )
  | Type(InputObject({iom_name})) =>
    base_type(generate_type_name(~prefix="t_variables", [iom_name]))
  | Type(Object(_)) =>
    raise(
      Location.Error(
        Location.error(~loc=loc |> conv_loc, "Object not allowed in args"),
      ),
    )
  | Type(Union(_)) =>
    raise(
      Location.Error(
        Location.error(~loc=loc |> conv_loc, "Union not allowed in args"),
      ),
    )
  | Type(Interface(_)) =>
    raise(
      Location.Error(
        Location.error(~loc=loc |> conv_loc, "Interface not allowed in args"),
      ),
    )
  | Nullable(inner) =>
    base_type(~inner=[generate_arg_type(loc, inner)], "option")
  | List(inner) =>
    base_type(~inner=[generate_arg_type(loc, inner)], "array")
  | TypeNotFound(name) =>
    raise(
      Location.Error(
        Location.error(
          ~loc=loc |> conv_loc,
          "Type " ++ name ++ " not found!",
        ),
      ),
    );

let generate_record_input_object = (input_obj_name, fields) => {
  Ast_helper.Type.mk(
    ~kind=
      Ptype_record(
        fields
        |> List.map(
             fun
             | InputField({name, type_, loc}) => {
                 Ast_helper.Type.field(
                   {Location.txt: name, loc: Location.none},
                   generate_arg_type(loc, type_),
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

let generate_object_input_object = (input_obj_name, fields) => {
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
                         generate_arg_type(loc, type_),
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
    (config: Generator_utils.output_config, input_obj_name, fields) => {
  config.records
    ? generate_record_input_object(input_obj_name, fields)
    : generate_object_input_object(input_obj_name, fields);
};

let generate_arg_types = (config, variable_defs) => {
  let input_objects = extract_args(config, variable_defs);

  input_objects
  |> List.map((InputObject({name, fields})) =>
       generate_input_object(config, name, fields)
     )
  |> Ast_helper.Str.type_(Recursive);
};

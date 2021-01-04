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
let rec generate_type = (~atLoc=?, ~config, ~path, ~raw) =>
  fun
  | Res_string(_) => base_type(~loc=?atLoc, "string")
  | Res_nullable({inner}) =>
    if (raw) {
      base_type(
        ~inner=[generate_type(~config, ~path, ~raw, inner)],
        "Js.Nullable.t",
      );
    } else {
      base_type(
        ~loc=?atLoc,
        ~inner=[generate_type(~config, ~path, ~raw, inner)],
        "option",
      );
    }
  | Res_array({inner}) =>
    base_type(
      ~loc=?atLoc,
      ~inner=[generate_type(~config, ~path, ~raw, inner)],
      "array",
    )
  | Res_custom_decoder({ident: module_name, inner}) =>
    if (raw) {
      generate_type(~config, ~path, ~raw, inner);
    } else {
      base_type(~loc=?atLoc, module_name ++ ".t");
    }
  | Res_id(_) => base_type(~loc=?atLoc, "string")
  | Res_int(_) => {
      base_type(~loc=?atLoc, "int");
    }
  | Res_float(_) => base_type(~loc=?atLoc, "float")
  | Res_boolean(_) => base_type(~loc=?atLoc, "bool")
  | Res_raw_scalar(_) =>
    base_type(
      ~loc=?atLoc,
      Ppx_config.native() ? "Yojson.Basic.t" : "Js.Json.t",
    )
  | Res_object({type_name})
  | Res_record({type_name}) =>
    switch (type_name, raw) {
    | (Some(type_name), false) => base_type(~loc=?atLoc, type_name)
    | (_, _) => base_type(~loc=?atLoc, generate_type_name(path))
    }
  | Res_poly_variant_selection_set(_)
  | Res_poly_variant_union(_) => {
      base_type(~loc=?atLoc, generate_type_name(path));
    }
  | Res_poly_variant_interface({name}) => {
      base_type(~loc=?atLoc, generate_type_name([name, ...path]));
    }
  | Res_solo_fragment_spread({loc, name: module_name}) =>
    if (raw) {
      base_type(~loc=conv_loc(loc), module_name ++ ".Raw.t");
    } else {
      base_type(~loc=conv_loc(loc), module_name ++ ".t");
    }
  | Res_error({loc, message: error}) =>
    raise(Location.Error(Location.error(~loc=conv_loc(loc), error)))
  | Res_poly_enum(_) => base_type(~loc=?atLoc, generate_type_name(path));

let wrap_type_declaration = (~loc as _=?, ~manifest=?, inner, path) => {
  Ast_helper.Type.mk(
    ~kind=inner,
    ~manifest?,
    {loc: Location.none, txt: generate_type_name(path)},
  );
};

let generate_opaque = (path, _loc: option(ast_location)) => {
  Ast_helper.Type.mk({loc: Location.none, txt: generate_type_name(path)});
};

let raw_opaque_object = (interface_fragments, fields) => {
  let has_fragments =
    fields
    |> List.exists(
         fun
         | Fragment(_) => true
         | _ => false,
       );
  switch (has_fragments, interface_fragments) {
  | (true, _) => true
  | (_, Some((_, []))) => false
  | (_, Some((_, _))) => true
  | _ => false
  };
};

let already_has__typename = fields =>
  List.exists(
    fun
    | Field({path: ["__typename", ..._]}) => true
    | _ => false,
    fields,
  );

let variant_interface_type =
    (
      ~name as interface_name=?,
      ~config,
      ~fragments,
      ~path,
      ~loc: option(ast_location),
      ~raw,
    ) => {
  let map_case_ty = ((name, res)) => {
    prf_desc:
      Rtag(
        {
          txt: name,
          loc:
            switch (loc) {
            | Some(loc) => conv_loc(loc)
            | None => Location.none
            },
        },
        false,
        [
          generate_type(
            ~config,
            ~path=
              switch (interface_name) {
              | Some(interface_name) => [name, interface_name, ...path]
              | None => [name, ...path]
              },
            ~raw,
            res,
          ),
        ],
      ),
    prf_loc:
      switch (loc) {
      | Some(loc) => conv_loc(loc)
      | None => Location.none
      },
    prf_attributes: [],
  };

  let fallback_case_ty = {
    prf_desc:
      Rtag(
        {
          txt: "UnspecifiedFragment",
          loc:
            switch (loc) {
            | Some(loc) => conv_loc(loc)
            | None => Location.none
            },
        },
        false,
        [base_type_name("string")],
      ),
    prf_loc:
      switch (loc) {
      | Some(loc) => conv_loc(loc)
      | None => Location.none
      },
    prf_attributes: [],
  };

  let fragment_case_tys = fragments |> List.map(map_case_ty);

  Ast_helper.(
    Typ.variant([fallback_case_ty, ...fragment_case_tys], Closed, None)
  );
};

let generate_variant_interface =
    (
      ~emit_locations,
      ~name,
      ~config,
      ~fragments,
      ~path,
      ~loc: ast_location,
      ~raw,
    ) =>
  wrap_type_declaration(
    Ptype_abstract,
    ~manifest=?
      raw
        ? None
        : Some(
            variant_interface_type(
              ~name,
              ~config,
              ~fragments,
              ~path,
              ~loc=emit_locations ? Some(loc) : None,
              ~raw,
            ),
          ),
    ~loc=?emit_locations ? Some(loc) : None,
    [name, ...path],
  );

/*
 * for given query:
 *
 * {
 *   user {
 *     id
 *     name
 *         loc_key
 *     |<---->|
 *     friends: {
 *       id
 *     }
 *     -----------
 *     With
 *       friends: {
 *         id
 *       }
 *     as the whole loc. They cannot be overlapping, so the most we can use for
 *     this is until the bracket
 *     -----------
 *   }
 * }
 *
 * the generated type is:
 *
 * type t = {
 *   user: option(t_user)
 * } and
 * t_user = {
 *   id: string
 *   friends: t_user_friends
 * } and
 * t_user_friends = {
 *   id: string
 * }
 *
 * When we add locations to the types, the types are not nested, so we cannot
 * add nested locations. The best way to model it is to just type the keys, and
 * set the location of the query to the module
 */

let generate_record_type =
    (
      ~config,
      ~obj_path,
      ~raw,
      ~loc: ast_location,
      ~is_variant,
      ~interface_fragments,
      ~emit_locations,
      fields,
    ) => {
  let record_fields =
    fields
    |> List.fold_left(
         acc =>
           fun
           | Fragment({key, loc_key, module_name, type_name}) => [
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
                     // this needs to be emitted both in the implementation and
                     // signature. Otherwise errors or hover types are not
                     // correct
                     loc: !raw ? conv_loc(loc_key) : Location.none,
                   },
                   [],
                 ),
               ),
               ...acc,
             ]
           | Field({path: [name, ...path], type_, loc_key, loc: _loc_field}) => {
               let valid_name = to_valid_ident(name);
               [
                 Ast_helper.(
                   Type.field(
                     ~loc=?{
                       emit_locations ? None : Some(conv_loc(loc_key));
                     },
                     ~attrs={
                       name == valid_name
                         ? []
                         : [
                           Ast_helper.Attr.mk(
                             {txt: "bs.as", loc: Location.none},
                             PStr([Str.eval(const_str_expr(name))]),
                           ),
                         ];
                     },
                     {
                       Location.txt: valid_name,
                       loc:
                         emit_locations ? Location.none : conv_loc(loc_key),
                     },
                     generate_type(
                       ~atLoc=?
                         emit_locations ? None : Some(conv_loc(loc_key)),
                       ~config,
                       ~path=[name, ...path],
                       ~raw,
                       type_,
                     ),
                   )
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
  let record_fields =
    switch (interface_fragments) {
    | Some((_, []))
    | None => record_fields
    | Some((interface_name, _fragments)) => [
        Ast_helper.Type.field(
          {Location.txt: "fragment", loc: Location.none},
          base_type(generate_type_name([interface_name, ...obj_path])),
        ),
        ...record_fields,
      ]
    };

  raw && raw_opaque_object(interface_fragments, fields)
    ? generate_opaque(obj_path, emit_locations ? Some(loc) : None)
    : wrap_type_declaration(
        Ptype_record(record_fields),
        ~loc={
          emit_locations ? Some(loc) : None;
        },
        obj_path,
      );
};

let generate_variant_selection =
    (~emit_locations, config, fields, path, loc: ast_location, raw) =>
  if (raw) {
    generate_opaque(path, emit_locations ? Some(loc) : None);
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
                           ~atLoc=?
                             emit_locations
                               ? Some(
                                   conv_loc(
                                     config.Generator_utils.map_loc(
                                       name.span,
                                     ),
                                   ),
                                 )
                               : None,
                           ~config,
                           ~path=[name.item, ...path],
                           ~raw,
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
      ~loc=?emit_locations ? Some(loc) : None,
      path,
    );
  };

let generate_variant_union =
    (
      ~emit_locations,
      config,
      fields: list((Result_structure.name, Result_structure.t)),
      omit_future_value,
      path,
      loc: ast_location,
      raw,
    ) =>
  if (raw) {
    generate_opaque(path, emit_locations ? Some(loc) : None);
  } else {
    let fallback_case_ty =
      omit_future_value
        ? []
        : [
          {
            prf_desc:
              Rtag(
                {
                  txt: "FutureAddedValue",
                  loc: emit_locations ? conv_loc(loc) : Location.none,
                },
                false,
                [base_type("Js.Json.t")],
              ),
            prf_loc: emit_locations ? conv_loc(loc) : Location.none,
            prf_attributes: [],
          },
        ];

    let fragment_case_tys =
      fields
      |> List.map(((name, res)) =>
           {
             prf_desc:
               Rtag(
                 {
                   txt: name.item,
                   loc: emit_locations ? conv_loc(loc) : Location.none,
                 },
                 false,
                 [
                   generate_type(
                     ~atLoc=?
                       emit_locations
                         ? Some(
                             conv_loc(
                               config.Generator_utils.map_loc(name.span),
                             ),
                           )
                         : None,
                     ~config,
                     ~path=[name.item, ...path],
                     ~raw,
                     res,
                   ),
                 ],
               ),
             prf_loc: emit_locations ? conv_loc(loc) : Location.none,
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
      ~loc=?emit_locations ? Some(conv_loc(loc)) : None,
      path,
    );
  };

let generate_enum =
    (
      ~emit_locations,
      _config,
      fields,
      path,
      ~loc: ast_location,
      raw,
      omit_future_value,
    ) =>
  wrap_type_declaration(
    Ptype_abstract,
    ~manifest=
      if (raw) {
        base_type("string");
      } else {
        [@metaloc emit_locations ? conv_loc(loc) : Location.none]
        Ast_helper.(
          Typ.variant(
            List.append(
              omit_future_value
                ? []
                : [
                  {
                    prf_desc:
                      Rtag(
                        {
                          txt: "FutureAddedValue",
                          loc: emit_locations ? conv_loc(loc) : Location.none,
                        },
                        false,
                        [base_type("string")],
                      ),
                    prf_loc: emit_locations ? conv_loc(loc) : Location.none,
                    prf_attributes: [],
                  },
                ],
              fields
              |> List.map(field =>
                   {
                     prf_desc:
                       Rtag(
                         {
                           txt: to_valid_ident(field),
                           loc:
                             emit_locations ? conv_loc(loc) : Location.none,
                         },
                         true,
                         [],
                       ),
                     prf_loc: emit_locations ? conv_loc(loc) : Location.none,
                     prf_attributes: [],
                   }
                 ),
            ),
            Closed,
            None,
          )
        );
      },
    ~loc,
    path,
  );

let generate_object_type =
    (
      ~emit_locations,
      config,
      fields,
      obj_path,
      raw,
      loc: ast_location,
      is_variant,
      interface_fragments,
    ) => {
  let object_fields =
    fields
    |> List.fold_left(
         acc =>
           fun
           | Fragment({key, loc_key, module_name, type_name}) => [
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
                         // this needs to be emitted both in the implementation and
                         // signature. Otherwise errors or hover types are not
                         // correct
                         loc: !raw ? conv_loc(loc_key) : Location.none,
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
                       ~atLoc=?
                         emit_locations ? None : Some(conv_loc(loc_key)),
                       ~config,
                       ~path=[name, ...path],
                       ~raw,
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
  let object_fields =
    switch (interface_fragments) {
    | Some((_, []))
    | None => object_fields
    | Some((interface_name, _fragments)) => [
        {
          pof_desc:
            Otag(
              {txt: "fragment", loc: Location.none},
              base_type(generate_type_name([interface_name, ...obj_path])),
            ),
          pof_loc: Location.none,
          pof_attributes: [],
        },
        ...object_fields,
      ]
    };

  raw && raw_opaque_object(interface_fragments, fields)
    ? generate_opaque(obj_path, emit_locations ? Some(loc) : None)
    : wrap_type_declaration(
        ~manifest=
          Ast_helper.(
            Typ.constr(
              {Location.txt: Longident.parse("Js.t"), loc: Location.none},
              [Ast_helper.Typ.object_(object_fields, Closed)],
            )
          ),
        Ptype_abstract,
        ~loc=?{
          emit_locations ? Some(loc) : None;
        },
        obj_path,
      );
};

let generate_graphql_object =
    (
      ~config: Generator_utils.output_config,
      ~obj_path,
      ~raw,
      ~emit_locations,
      ~loc,
      ~is_variant,
      ~type_name,
      ~interface_fragments,
      fields,
    ) => {
  switch (type_name) {
  | Some(type_name) =>
    wrap_type_declaration(
      ~manifest=base_type(type_name),
      Ptype_abstract,
      ~loc=?{
        emit_locations ? Some(loc) : None;
      },
      obj_path,
    )
  | None =>
    generate_record_type(
      ~emit_locations,
      ~config,
      ~obj_path,
      ~raw,
      ~loc,
      ~is_variant,
      ~interface_fragments,
      fields,
    )
  };
};

let generate_types =
    (
      ~config: Generator_utils.output_config,
      ~raw,
      ~emit_locations,
      ~type_name,
      ~fragment_name,
      res,
    ) => {
  extract(~fragment_def=Option.is_some(fragment_name), ~path=[], ~raw, res)
  |> List.map(
       fun
       | Object({
           fields,
           path: obj_path,
           loc,
           variant_parent,
           interface_fragments,
         }) =>
         generate_graphql_object(
           ~config,
           ~obj_path,
           ~raw,
           ~loc,
           ~emit_locations,
           ~is_variant=variant_parent,
           ~type_name,
           ~interface_fragments,
           fields,
         )
       | VariantSelection({loc, path, fields}) =>
         generate_variant_selection(
           ~emit_locations,
           config,
           fields,
           path,
           loc,
           raw,
         )
       | VariantUnion({loc, path, fields, omit_future_value}) =>
         generate_variant_union(
           ~emit_locations,
           config,
           fields,
           omit_future_value,
           path,
           loc,
           raw,
         )
       | VariantInterface({name, loc, path, fragments}) =>
         generate_variant_interface(
           ~emit_locations,
           ~name,
           ~config,
           ~fragments,
           ~path,
           ~loc,
           ~raw,
         )
       | Enum({loc, path, fields, omit_future_value}) =>
         generate_enum(
           ~emit_locations,
           config,
           fields,
           path,
           ~loc,
           raw,
           omit_future_value,
         ),
     )
  |> List.rev;
};

let make_fragment_type =
    (config, raw, type_name, fragment_name, fragment_name_loc) => {
  Ast_helper.(
    Type.mk(
      ~manifest=
        Typ.constr(
          raw
            ? Location.mknoloc(Longident.Lident("t"))
            : Location.mkloc(
                switch (type_name) {
                | Some(type_name) => Longident.parse(type_name)
                | None => Longident.Lident("t")
                },
                switch (fragment_name_loc) {
                | Some(fragment_name_loc) =>
                  conv_loc(config.Generator_utils.map_loc(fragment_name_loc))
                | None => Location.none
                },
              ),
          [],
        ),
      Location.mknoloc("t_" ++ fragment_name),
    )
  );
};
// generate all the types necessary types that we later refer to by name.
let generate_type_structure_items =
    (config, res, raw, type_name, fragment_name) => {
  let types =
    generate_types(
      ~config,
      ~emit_locations=false,
      ~raw,
      ~type_name,
      ~fragment_name,
      res,
    )
    |> List.map(type_ => Ast_helper.Str.type_(Recursive, [type_]));

  switch (fragment_name) {
  | Some((fragment_name, _fragment_name_loc)) =>
    List.append(
      types,
      [
        Ast_helper.(
          Str.type_(
            Nonrecursive,
            [
              make_fragment_type(config, raw, type_name, fragment_name, None),
            ],
          )
        ),
      ],
    )
  | None => types
  };
};
let generate_type_signature_items =
    (
      config: Generator_utils.output_config,
      res,
      raw,
      type_name,
      fragment_name,
    ) => {
  let emit_locations = raw ? false : true;
  let types =
    generate_types(
      ~config,
      ~emit_locations,
      ~raw,
      ~type_name,
      ~fragment_name,
      res,
    )
    |> List.map(type_ => Ast_helper.Sig.type_(Recursive, [type_]));

  switch (fragment_name) {
  | Some((fragment_name, fragment_name_loc)) =>
    List.append(
      types,
      [
        Ast_helper.(
          Sig.type_(
            Nonrecursive,
            [
              make_fragment_type(
                config,
                raw,
                type_name,
                fragment_name,
                emit_locations ? Some(fragment_name_loc) : None,
              ),
            ],
          )
        ),
      ],
    )
  | None => types
  };
};

let rec generate_arg_type = (~nulls=true, raw, originalLoc) => {
  let loc = raw ? None : Some(conv_loc(originalLoc));
  fun
  | Type(Scalar({sm_name: "ID"}))
  | Type(Scalar({sm_name: "String"})) => base_type(~loc?, "string")
  | Type(Scalar({sm_name: "Int"})) => base_type(~loc?, "int")
  | Type(Scalar({sm_name: "Float"})) => base_type(~loc?, "float")
  | Type(Scalar({sm_name: "Boolean"})) => base_type(~loc?, "bool")
  | Type(Scalar({sm_name: _})) =>
    base_type(~loc?, Ppx_config.native() ? "Yojson.Basic.t" : "Js.Json.t")
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
    nulls
      ? base_type(
          ~loc?,
          ~inner=[
            generate_arg_type(raw, conv_loc_from_ast(Location.none), inner),
          ],
          raw ? "Js.Nullable.t" : "option",
        )
      : generate_arg_type(raw, conv_loc_from_ast(Location.none), inner)
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

let generate_empty_input_object = () => {
  Ast_helper.Type.mk(
    ~manifest=base_type_name("unit"),
    {loc: Location.none, txt: generate_type_name(~prefix="t_variables", [])},
  );
};

let generate_record_input_object = (raw, input_obj_name, fields) => {
  Ast_helper.(
    Type.mk(
      ~kind=
        Ptype_record(
          fields
          |> List.map(
               fun
               | InputField({name, type_, loc}) => {
                   let valid_name = to_valid_ident(name);
                   Ast_helper.Type.field(
                     ~attrs=
                       if (valid_name == name) {
                         [];
                       } else {
                         Ppx_config.native()
                           ? []
                           : [
                             Ast_helper.Attr.mk(
                               {txt: "bs.as", loc: Location.none},
                               PStr([Str.eval(const_str_expr(name))]),
                             ),
                           ];
                       },
                     {
                       Location.txt: valid_name,
                       loc: raw ? Location.none : conv_loc(loc),
                     },
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
    )
  );
};

let generate_input_object = (raw, _, input_obj_name, fields) => {
  generate_record_input_object(raw, input_obj_name, fields);
};

let generate_arg_type_structure_items = (raw, config, variable_defs) => {
  let input_objects = extract_args(config, variable_defs);
  [
    input_objects
    |> List.map(
         fun
         | NoVariables => generate_empty_input_object()
         | InputObject({name, fields}) => {
             generate_input_object(raw, config, name, fields);
           },
       )
    |> Ast_helper.Str.type_(Recursive),
  ];
};
let generate_arg_type_signature_items = (raw, config, variable_defs) => {
  let input_objects = extract_args(config, variable_defs);
  [
    input_objects
    |> List.map(
         fun
         | NoVariables => generate_empty_input_object()
         | InputObject({name, fields}) => {
             generate_input_object(raw, config, name, fields);
           },
       )
    |> Ast_helper.Sig.type_(Recursive),
  ];
};

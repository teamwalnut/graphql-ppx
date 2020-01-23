open Graphql_ppx_base;
open Result_structure;
open Extract_type_definitions;
open Source_pos;
open Output_bucklescript_utils;

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

let generate_name = (~prefix="t") =>
  fun
  | [] => prefix
  | path => {
      path
      |> List.rev
      |> List.fold_left((acc, item) => acc ++ "_" ++ item, prefix);
    };

let base_type = (~inner=[], name) => {
  Ast_helper.Typ.constr(
    {Location.txt: Longident.parse(name), loc: Location.none},
    inner,
  );
};

// generate the type definition, including nullables, arrays etc.
let rec generate_type = (config, path) =>
  fun
  | Res_string(loc) => base_type("string")
  | Res_nullable(_loc, inner) =>
    base_type(~inner=[generate_type(config, path, inner)], "option")
  | Res_array(_loc, inner) =>
    base_type(~inner=[generate_type(config, path, inner)], "array")
  | Res_custom_decoder(loc, module_name, _) => base_type(module_name ++ ".t")
  | Res_id(loc) => base_type("string")
  | Res_int(loc) => base_type("int")
  | Res_float(loc) => base_type("float")
  | Res_boolean(loc) => base_type("bool")
  | Res_raw_scalar(loc) => base_type("Js.Json.t")
  | Res_object(_loc, name, _fields)
  | Res_record(_loc, name, _fields) => base_type(generate_name(path))
  | Res_poly_variant_union(loc, name, fragments, exhaustive_flag) => {
      let (fallback_case, fallback_case_ty) =
        Ast_406.Parsetree.(
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
                  Rtag(
                    {txt: "Nonexhaustive", loc: conv_loc(loc)},
                    [],
                    true,
                    [],
                  ),
                ],
              )
            }
          )
        );

      let fragment_case_tys =
        fragments
        |> List.map(((name, res)) =>
             Ast_406.Parsetree.(
               Rtag(
                 {txt: name, loc: conv_loc(loc)},
                 [],
                 false,
                 [generate_type(config, [name, ...path], res)],
               )
             )
           );
      Ast_helper.(
        Typ.variant(
          List.concat([fallback_case_ty, fragment_case_tys]),
          Closed,
          None,
        )
      );
    }
  | Res_solo_fragment_spread(loc, module_name) =>
    base_type(module_name ++ ".t")
  | Res_poly_variant_interface(loc, name, base, fragments) => {
      let map_case_ty = ((name, res)) =>
        Ast_406.Parsetree.(
          Rtag(
            {txt: name, loc: conv_loc(loc)},
            [],
            false,
            [generate_type(config, [name, ...path], res)],
          )
        );

      let fallback_case_ty = map_case_ty(base);
      let fragment_case_tys = fragments |> List.map(map_case_ty);

      Ast_helper.(
        Typ.variant([fallback_case_ty, ...fragment_case_tys], Closed, None)
      );
    }
  | Res_error(loc, error) =>
    raise(Location.Error(Location.error(~loc=conv_loc(loc), error)))
  | Res_poly_enum(loc, enum_meta) => {
      Ast_406.Parsetree.(
        Graphql_ppx_base__.Schema.(
          [@metaloc conv_loc(loc)]
          Ast_helper.(
            Typ.variant(
              enum_meta.em_values
              |> List.map(({evm_name, _}) =>
                   Rtag({txt: evm_name, loc: conv_loc(loc)}, [], true, [])
                 ),
              Closed,
              None,
            )
          )
        )
      );
    };

let generate_record_type = (config, fields, obj_path) => {
  Ast_helper.Type.mk(
    ~kind=
      Ptype_record(
        fields
        |> List.map(
             fun
             | Fragment({key, module_name, type_name}) =>
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
               )
             | Field({path: [name, ...path], type_}) =>
               Ast_helper.Type.field(
                 {Location.txt: name, loc: Location.none},
                 generate_type(config, [name, ...path], type_),
               )
             | Field({path: [], loc}) =>
               // I don't think this should ever happen but we need to
               // cover this case, perhaps we can constrain the type
               raise(
                 Location.Error(
                   Location.error(~loc=loc |> conv_loc, "No path"),
                 ),
               ),
           ),
      ),
    {loc: Location.none, txt: generate_name(obj_path)},
  );
};

let generate_object_type = (config, fields, obj_path) => {
  Parsetree.(
    Ast_helper.(
      Type.mk(
        ~kind=Ptype_abstract,
        ~manifest=
          Typ.constr(
            {Location.txt: Longident.parse("Js.t"), loc: Location.none},
            [
              Ast_helper.Typ.object_(
                fields
                |> List.map(
                     fun
                     | Fragment({key, module_name, type_name}) =>
                       Otag(
                         {txt: key, loc: Location.none},
                         [],
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
                       )
                     | Field({path: [name, ...path], type_}) =>
                       Otag(
                         {txt: name, loc: Location.none},
                         [],
                         generate_type(config, [name, ...path], type_),
                       )
                     | Field({path: [], loc}) =>
                       // I don't think this should ever happen but we need to
                       // cover this case, perhaps we can constrain the type
                       raise(
                         Location.Error(
                           Location.error(~loc=loc |> conv_loc, "No path"),
                         ),
                       ),
                   ),
                Closed,
              ),
            ],
          ),
        {loc: Location.none, txt: generate_name(obj_path)},
      )
    )
  );
};
let generate_graphql_object =
    (config: Generator_utils.output_config, fields, obj_path, force_record) => {
  config.records || force_record
    ? generate_record_type(config, fields, obj_path)
    : generate_object_type(config, fields, obj_path);
};

// generate all the types necessary types that we later refer to by name.
let generate_types = (config: Generator_utils.output_config, res) => {
  let types =
    extract([], res)
    |> List.map(
         fun
         | Object({fields, path: obj_path, force_record}) =>
           generate_graphql_object(config, fields, obj_path, force_record),
       );

  Ast_helper.Str.type_(Recursive, types);
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
    Ast_406.Parsetree.(
      Graphql_ppx_base__.Schema.(
        Ast_helper.(
          Typ.variant(
            enum_meta.em_values
            |> List.map(({evm_name, _}) =>
                 Rtag({txt: evm_name, loc: Location.none}, [], true, [])
               ),
            Closed,
            None,
          )
        )
      )
    )
  | Type(InputObject({iom_name})) =>
    base_type(generate_name(~prefix="t_variables", [iom_name]))
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
        generate_name(
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
  Parsetree.(
    Ast_helper.(
      Type.mk(
        ~kind=Ptype_abstract,
        ~manifest=
          Typ.constr(
            {Location.txt: Longident.parse("Js.t"), loc: Location.none},
            [
              Ast_helper.Typ.object_(
                fields
                |> List.map(
                     fun
                     | InputField({name, type_, loc}) =>
                       Otag(
                         {txt: name, loc: Location.none},
                         [],
                         generate_arg_type(loc, type_),
                       ),
                   ),
                Closed,
              ),
            ],
          ),
        {
          loc: Location.none,
          txt:
            generate_name(
              ~prefix="t_variables",
              switch (input_obj_name) {
              | None => []
              | Some(name) => [name]
              },
            ),
        },
      )
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
  |> List.map(
       fun
       | InputObject({name, fields}) => {
           generate_input_object(config, name, fields);
         },
     )
  |> Ast_helper.Str.type_(Recursive);
};

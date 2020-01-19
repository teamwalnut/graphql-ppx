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

let generate_name =
  fun
  | [] => "t"
  | path => {
      path
      |> List.rev
      |> List.fold_left((acc, item) => acc ++ "_" ++ item, "t");
    };

let base_type = name => {
  Ast_helper.Typ.constr(
    {Location.txt: Longident.parse(name), loc: Location.none},
    [],
  );
};

// generate the type definition, including nullables, arrays etc.
let rec generate_type = path =>
  fun
  | Res_string(loc) => base_type("string")
  | Res_nullable(_loc, inner) =>
    Ast_helper.(
      Typ.constr(
        {Location.txt: Longident.Lident("option"), loc: Location.none},
        [generate_type(path, inner)],
      )
    )
  | Res_array(_loc, inner) =>
    Ast_helper.(
      Typ.constr(
        {Location.txt: Longident.Lident("array"), loc: Location.none},
        [generate_type(path, inner)],
      )
    )
  | Res_custom_decoder(loc, module_name, _) => base_type(module_name ++ ".t")
  | Res_id(loc) => base_type("string")
  | Res_int(loc) => base_type("int")
  | Res_float(loc) => base_type("float")
  | Res_boolean(loc) => base_type("bool")
  | Res_object(_loc, name, _fields)
  | Res_record(_loc, name, _fields) => base_type(generate_name(path))
  | Res_poly_variant_selection_set(loc, name, fields) =>
    Ast_406.Parsetree.(
      Ast_helper.(
        Typ.variant(
          fields
          |> List.map(((name, _)) =>
               Rtag(
                 {txt: Compat.capitalize_ascii(name), loc: conv_loc(loc)},
                 [],
                 false,
                 [
                   {
                     ptyp_desc: Ptyp_any,
                     ptyp_attributes: [],
                     ptyp_loc: Location.none,
                   },
                 ],
               )
             ),
          Closed,
          None,
        )
      )
    )
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
                 [generate_type([name, ...path], res)],
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
  | Res_poly_variant_interface(loc, name, base, fragments) => {
      let map_case_ty = ((name, res)) =>
        Ast_406.Parsetree.(
          Rtag(
            {txt: name, loc: conv_loc(loc)},
            [],
            false,
            [generate_type([name, ...path], res)],
          )
        );

      let fallback_case_ty = map_case_ty(base);
      let fragment_case_tys = fragments |> List.map(map_case_ty);

      Ast_helper.(
        Typ.variant([fallback_case_ty, ...fragment_case_tys], Closed, None)
      );
    }
  | Res_solo_fragment_spread(loc, _)
  | Res_error(loc, _)
  | Res_raw_scalar(loc)
  | Res_poly_enum(loc, _) =>
    raise(
      Location.Error(
        Location.error(~loc=conv_loc(loc), "Currently unsupported"),
      ),
    );

// generate all the types necessary types that we later refer to by name.
let generate_types = (path, res) => {
  let types =
    extract(path, res)
    |> List.map(
         fun
         | Object({fields, path: obj_path}) =>
           Ast_helper.Type.mk(
             ~kind=
               Ptype_record(
                 fields
                 |> List.map(
                      fun
                      | Fragment({module_name}) =>
                        Ast_helper.Type.field(
                          {
                            Location.txt: "fragment_" ++ module_name,
                            loc: Location.none,
                          },
                          Ast_helper.Typ.constr(
                            {
                              Location.txt:
                                Longident.parse(module_name ++ ".t"),
                              loc: Location.none,
                            },
                            [],
                          ),
                        )
                      | Field({path: [name, ...path], type_}) =>
                        Ast_helper.Type.field(
                          {Location.txt: name, loc: Location.none},
                          generate_type([name, ...path], type_),
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
           ),
       );

  Ast_helper.Str.type_(Recursive, types);
};

open Graphql_ppx_base;
open Result_structure;
open Extract_type_definitions;
open Source_pos;
open Output_bucklescript_utils;

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
    {Location.txt: Longident.Lident(name), loc: Location.none},
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
  | Res_id(loc) => base_type("string")
  | Res_int(loc) => base_type("int")
  | Res_float(loc) => base_type("float")
  | Res_boolean(loc) => base_type("bool")
  | Res_object(_loc, name, _fields)
  | Res_record(_loc, name, _fields) => base_type(generate_name(path))
  | Res_poly_variant_selection_set(loc, _, _)
  | Res_poly_variant_union(loc, _, _, _)
  | Res_poly_variant_interface(loc, _, _, _)
  | Res_solo_fragment_spread(loc, _)
  | Res_error(loc, _)
  | Res_raw_scalar(loc)
  | Res_custom_decoder(loc, _, _)
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
                      // need to raise here on an empty list, but that should never happen
                      | Field({path: [name, ...path], type_}) =>
                        Ast_helper.Type.field(
                          {Location.txt: name, loc: Location.none},
                          generate_type([name, ...path], type_),
                        ),
                    ),
               ),
             {loc: Location.none, txt: generate_name(obj_path)},
           ),
       );

  Ast_helper.Str.type_(Recursive, types);
};

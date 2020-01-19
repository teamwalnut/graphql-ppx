open Result_structure;
open Graphql_ppx_base__;

type object_field =
  | Field({
      type_: Result_structure.t,
      loc: Source_pos.ast_location,
      path: list(string),
    })
  | Fragment({
      module_name: string,
      key: string,
      type_name: option(string),
    });

type type_def =
  | Object({
      path: list(string),
      fields: list(object_field),
    });

// function that generate types. It will output a nested list type descriptions
// later this result can be flattened and converted to an ast of combined type
// definitions
let rec extract = path =>
  fun
  | Res_nullable(loc, inner) => extract(path, inner)
  | Res_array(loc, inner) => extract(path, inner)
  | Res_object(loc, obj_name, fields)
  | Res_record(loc, obj_name, fields) => {
      // let path = List.length(path) == 0 ? [obj_name] : path;
      let path = path;
      [
        Object({
          path,
          fields:
            fields
            |> List.map(
                 fun
                 | Fr_named_field(name, loc, type_) =>
                   Field({loc, path: [name, ...path], type_})
                 | Fr_fragment_spread(key, _loc, name, type_name) =>
                   Fragment({module_name: name, key, type_name}),
               ),
        }),
        ...fields
           |> List.fold_left(
                acc =>
                  fun
                  | Fr_named_field(name, _loc, type_) =>
                    List.append(extract([name, ...path], type_), acc)
                  | Fr_fragment_spread(_key, _loc, _name, _) => acc,
                [],
              ),
      ];
    }
  | Res_poly_variant_union(loc, name, fragments, _)
  | Res_poly_variant_interface(loc, name, _, fragments) =>
    fragments
    |> List.fold_left(
         (acc, (name, inner)) =>
           List.append(extract([name, ...path], inner), acc),
         [],
       )
  | Res_custom_decoder(loc, ident, inner) => extract(path, inner)
  | Res_solo_fragment_spread(loc, name) => []
  | Res_error(loc, message) => []
  | Res_id(loc) => []
  | Res_string(loc) => []
  | Res_int(loc) => []
  | Res_float(loc) => []
  | Res_boolean(loc) => []
  | Res_raw_scalar(_) => []
  | Res_poly_enum(loc, enum_meta) => [];

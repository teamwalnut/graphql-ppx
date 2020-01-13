// function that generate types. It will output a nested list type descriptions,
// (probably as variants with parameters), later be flattened and converted to
// an ast of combined type definitions

// thing to note.. we need to capture the "path" for naming
let rec generate_types = config =>
  fun
  | Res_nullable(loc, inner) => [generate_types(inner)]
  | Res_array(loc, inner) => [generate_types(inner)]
  | Res_poly_enum(loc, enum_meta) => []
  | Res_record(loc, name, fields) => []
  | Res_object(loc, name, fields) => []
  | Res_poly_variant_selection_set(loc, name, fields) => []
  | Res_poly_variant_union(loc, name, fragments, exhaustive) => []
  | Res_poly_variant_interface(loc, name, base, fragments) => []
  | Res_solo_fragment_spread(loc, name) => []
  | Res_error(loc, message) => []
  | Res_id(loc) => []
  | Res_string(loc) => []
  | Res_int(loc) => []
  | Res_float(loc) => []
  | Res_boolean(loc) => []
  | Res_raw_scalar(_) => []
  | Res_custom_decoder(loc, ident, inner) => [];

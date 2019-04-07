type exhaustive_flag =
  | Exhaustive
  | Nonexhaustive;

type loc = Source_pos.ast_location;

type field_result =
  | Fr_named_field(string, loc, t)
  | Fr_fragment_spread(string, loc, string)
and typename_set = {
  res_structure: t,
  poly_variant_name: string,
  typename: string,
}
and t =
  | Res_nullable(loc, t)
  | Res_array(loc, t)
  | Res_id(loc)
  | Res_string(loc)
  | Res_int(loc)
  | Res_float(loc)
  | Res_boolean(loc)
  | Res_raw_scalar(loc)
  | Res_poly_enum(loc, Schema.enum_meta)
  | Res_custom_decoder(loc, string, t)
  | Res_record(loc, string, list(field_result))
  | Res_object(loc, string, list(field_result))
  | Res_poly_variant_selection_set(loc, string, list(typename_set))
  | Res_poly_variant_union(loc, string, list(typename_set), exhaustive_flag)
  | Res_poly_variant_interface(loc, string, typename_set, list(typename_set))
  | Res_solo_fragment_spread(loc, string)
  | Res_error(loc, string);

type mod_ =
  | Mod_fragment(
      string,
      list(string),
      bool,
      Source_pos.spanning(Graphql_ast.fragment),
      t,
    )
  | Mod_default_operation(
      option(Source_pos.spanning(Graphql_ast.variable_definitions)),
      bool,
      Source_pos.spanning(Graphql_ast.operation),
      t,
    );

let res_loc =
  fun
  | Res_nullable(loc, _)
  | Res_array(loc, _)
  | Res_id(loc)
  | Res_string(loc)
  | Res_int(loc)
  | Res_float(loc)
  | Res_boolean(loc)
  | Res_raw_scalar(loc)
  | Res_poly_enum(loc, _)
  | Res_custom_decoder(loc, _, _)
  | Res_record(loc, _, _)
  | Res_object(loc, _, _)
  | Res_poly_variant_selection_set(loc, _, _)
  | Res_poly_variant_union(loc, _, _, _)
  | Res_poly_variant_interface(loc, _, _, _)
  | Res_solo_fragment_spread(loc, _)
  | Res_error(loc, _) => loc;

let can_be_absent_as_field =
  fun
  | Res_nullable(_) => true
  | _ => false;

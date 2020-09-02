type exhaustive_flag =
  | Exhaustive
  | Nonexhaustive;

type loc = Source_pos.ast_location;
type name = Source_pos.spanning(string);

type field_result =
  | Fr_named_field({
      name: string,
      loc_key: loc,
      loc,
      type_: t,
      arguments: Graphql_ast.arguments,
    })
  | Fr_fragment_spread({
      key: string,
      loc,
      name: string,
      type_name: option(string),
      arguments: list(string),
    })
and t =
  | Res_nullable({
      loc,
      inner: t,
    })
  | Res_array({
      loc,
      inner: t,
    })
  | Res_id({loc})
  | Res_string({loc})
  | Res_int({loc})
  | Res_float({loc})
  | Res_boolean({loc})
  | Res_raw_scalar({loc})
  | Res_poly_enum({
      loc,
      enum_meta: Schema.enum_meta,
      omit_future_value: bool,
    })
  | Res_custom_decoder({
      loc,
      ident: string,
      inner: t,
    })
  | Res_record({
      loc,
      name: string,
      fields: list(field_result),
      type_name: option(string),
      interface_fragments: option((string, list((string, t)))),
    })
  | Res_object({
      loc,
      name: string,
      fields: list(field_result),
      type_name: option(string),
      interface_fragments: option((string, list((string, t)))),
    })
  | Res_poly_variant_selection_set({
      loc,
      name: string,
      fragments: list((name, t)),
    })
  | Res_poly_variant_union({
      loc,
      name: string,
      fragments: list((name, t)),
      exhaustive: exhaustive_flag,
      omit_future_value: bool,
    })
  | Res_poly_variant_interface({
      loc,
      name: string,
      fragments: list((string, t)),
    })
  | Res_solo_fragment_spread({
      loc,
      name: string,
      arguments: list(string),
    })
  | Res_error({
      loc,
      message: string,
    });

type definition =
  | Def_fragment({
      name: string,
      variable_definitions:
        option(Source_pos.spanning(Graphql_ast.variable_definitions)),
      has_error: bool,
      fragment: Source_pos.spanning(Graphql_ast.fragment),
      type_name: option(string), // decodes to a custom type (ppxAs)
      inner: t,
    })
  | Def_operation({
      variable_definitions:
        option(Source_pos.spanning(Graphql_ast.variable_definitions)),
      has_error: bool,
      operation: Source_pos.spanning(Graphql_ast.operation),
      inner: t,
    });

let res_loc: t => loc;
let can_be_absent_as_field: t => bool;

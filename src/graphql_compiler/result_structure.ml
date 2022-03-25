type exhaustive_flag = Exhaustive | Nonexhaustive
type loc = Source_pos.ast_location
type name = string Source_pos.spanning

type field_result =
  | Fr_named_field of {
      name : string;
      loc_key : loc;
      loc : loc;
      type_ : t;
      arguments : Graphql_ast.arguments;
    }
  | Fr_fragment_spread of {
      key : string;
      loc : loc;
      name : string;
      type_name : string option;
      arguments : string list;
    }

and t =
  | Res_nullable of { loc : loc; inner : t }
  | Res_array of { loc : loc; inner : t }
  | Res_id of { loc : loc }
  | Res_string of { loc : loc }
  | Res_int of { loc : loc }
  | Res_float of { loc : loc }
  | Res_boolean of { loc : loc }
  | Res_raw_scalar of { loc : loc }
  | Res_poly_enum of {
      loc : loc;
      enum_meta : Schema.enum_meta;
      omit_future_value : bool;
    }
  | Res_custom_decoder of { loc : loc; ident : string; inner : t }
  | Res_record of {
      loc : loc;
      name : string;
      fields : field_result list;
      type_name : string option;
      interface_fragments : (string * (string * t) list) option;
    }
  | Res_object of {
      loc : loc;
      name : string;
      fields : field_result list;
      type_name : string option;
      interface_fragments : (string * (string * t) list) option;
    }
  | Res_poly_variant_selection_set of {
      loc : loc;
      name : string;
      fragments : (name * t) list;
    }
  | Res_poly_variant_union of {
      loc : loc;
      name : string;
      fragments : (name * t) list;
      exhaustive : exhaustive_flag;
      omit_future_value : bool;
    }
  | Res_poly_variant_interface of {
      loc : loc;
      name : string;
      fragments : (string * t) list;
    }
  | Res_solo_fragment_spread of {
      loc : loc;
      name : string;
      type_name : string;
      arguments : string list;
    }
  | Res_error of { loc : loc; message : string }

type definition =
  | Def_fragment of {
      name : string;
      variable_definitions :
        Graphql_ast.variable_definitions Source_pos.spanning option;
      has_error : bool;
      fragment : Graphql_ast.fragment Source_pos.spanning;
      type_name : string option;
      inner : t;
    }
  | Def_operation of {
      variable_definitions :
        Graphql_ast.variable_definitions Source_pos.spanning option;
      has_error : bool;
      operation : Graphql_ast.operation Source_pos.spanning;
      inner : t;
    }

let res_loc = function
  | Res_nullable { loc }
  | Res_array { loc }
  | Res_id { loc }
  | Res_string { loc }
  | Res_int { loc }
  | Res_float { loc }
  | Res_boolean { loc }
  | Res_raw_scalar { loc }
  | Res_poly_enum { loc }
  | Res_custom_decoder { loc }
  | Res_record { loc }
  | Res_object { loc }
  | Res_poly_variant_selection_set { loc }
  | Res_poly_variant_union { loc }
  | Res_poly_variant_interface { loc }
  | Res_solo_fragment_spread { loc }
  | Res_error { loc } ->
    loc

let can_be_absent_as_field = function Res_nullable _ -> true | _ -> false

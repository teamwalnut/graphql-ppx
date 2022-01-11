type error_marker = { mutable has_error : bool }

let raise_error_with_loc = Ppx_config.raise_error_with_loc

let raise_error map_loc span message =
  raise_error_with_loc (map_loc span) message

let some_or o d = match o with Some v -> v | None -> d
let capitalize_ascii = String.capitalize_ascii
let uncapitalize_ascii = String.uncapitalize_ascii

type output_config = {
  map_loc :
    Source_pos.source_position * Source_pos.source_position ->
    Source_pos.ast_location;
  delimiter : string option;
  schema : Schema.t;
  full_document : Graphql_ast.document;
  template_tag : string option * string option * string option;
  template_tag_return_type : string option;
  template_tag_is_function : bool option;
  inline : bool;
  future_added_value : bool;
  extend : string option;
  fragment_in_query : Ppx_config.fragment_in_query;
  native : bool;
}

let filter_map f l =
  let rec loop acc = function
    | [] -> List.rev acc
    | head :: tail -> (
        match f head with
        | None -> loop acc tail
        | Some v -> loop (v :: acc) tail)
  in
  loop [] l

exception Cant_find_fragment_type of string Source_pos.spanning

type t =
  | String of string
  | FragmentNameRef of string
  | FragmentQueryRef of string

val type_ref_name : Schema.type_ref -> string
val print_type : Graphql_ast.type_ref -> string
val print_document : Schema.t -> Graphql_ast.document -> t array
val print_input_value : Graphql_ast.input_value -> string

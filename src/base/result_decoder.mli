type query_config = {
  schema : string option;
  records : bool option;
  objects : bool option;
  inline : bool option;
  template_tag : string option;
  template_tag_location : string option;
  template_tag_import : string option;
  template_tag_return_type : string option;
  tagged_template : bool option;
  template_tag_is_function : bool option;
  future_added_value : bool option;
  extend : string option;
  fragment_in_query : Ppx_config.fragment_in_query option;
  apollo_mode : bool option;
}

val generate_config :
  json_read_fn:(string -> Read_schema.Json.t) ->
  map_loc:
    (Source_pos.source_position * Source_pos.source_position ->
    Source_pos.ast_location) ->
  delimiter:string option ->
  initial_query_config:query_config ->
  Graphql_ast.definition list ->
  (Graphql_ast.definition * Generator_utils.output_config) list

val unify_document_schema :
  (Graphql_ast.definition * Generator_utils.output_config) list ->
  (Result_structure.definition * Generator_utils.output_config) list
(**
  Takes a list of the AST of the GraphQL definitions (operations or \
  fragments) and transforms that into a list of result structures. These result \
  structures form the basis to generate the reason code for:

    - `query` variable
    - `parse` function
    - `serialize` function
    - `makeVariables` function
*)

let getFragmentArgumentDefinitions:
  list(Source_pos.spanning(Graphql_ast.directive)) =>
  list((string, string, Source_pos.span, Source_pos.span));

type query_config = {
  schema: option(string),
  records: option(bool),
  objects: option(bool),
  inline: option(bool),
  template_tag: option(string),
  template_tag_location: option(string),
  template_tag_import: option(string),
  template_tag_return_type: option(string),
  tagged_template: option(bool),
  future_added_value: option(bool),
  extend: option(string),
  fragment_in_query: option(Ppx_config.fragment_in_query),
  apollo_mode: option(bool),
};

let generate_config:
  (
    ~schema: Schema.t,
    ~map_loc: ((Source_pos.source_position, Source_pos.source_position)) =>
              Source_pos.ast_location,
    ~delimiter: option(string),
    ~initial_query_config: query_config,
    list(Graphql_ast.definition)
  ) =>
  list((Graphql_ast.definition, Generator_utils.output_config));
/**
 * Takes a list of the AST of the GraphQL definitions (operations or fragments)
 * and transforms that into a list of result structures. These result structures
 * form the basis to generate the reason code for:
 * - `query` variable
 * - `parse` function
 * - `serialize` function
 * - `makeVariables` function
*/
let unify_document_schema:
  list((Graphql_ast.definition, Generator_utils.output_config)) =>
  list((Result_structure.definition, Generator_utils.output_config));

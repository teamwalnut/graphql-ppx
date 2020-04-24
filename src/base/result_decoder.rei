let getFragmentArgumentDefinitions:
  list(Source_pos.spanning(Graphql_ast.directive)) =>
  list((string, string, Source_pos.span, Source_pos.span));

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
  (Generator_utils.output_config, list(Graphql_ast.definition)) =>
  list(Result_structure.definition);

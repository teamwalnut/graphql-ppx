type t =
  | String(string)
  | FragmentNameRef(string)
  | FragmentQueryRef(string);

let print_type: Graphql_ast.type_ref => string;
let print_document:
  (Schema.schema, list(Graphql_ast.definition)) => array(t);

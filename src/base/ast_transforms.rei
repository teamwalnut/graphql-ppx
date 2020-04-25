type t = (Schema.schema, Graphql_ast.document) => Graphql_ast.document;

let add_typename_to_selection_set: t;
let remove_typename_from_union: t;

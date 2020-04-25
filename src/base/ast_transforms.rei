/**
 * An AST transform will take the schema and a document, and transform the AST
 * of the document returning the same document type
 *
 * With these transforms we can slightly change the GraphQL operation or
 * fragment compared to what the user supplies. This is helpful for instance
 * to add fields that clients will add implicitly, or to preprocess the document
 * to conform to some of the conventions of GraphQL PPX.
 */
type t = (Schema.schema, Graphql_ast.document) => Graphql_ast.document;

/**
 * This is a GraphQL AST transform that will add the __typename to all selection
 * sets. This is necessary for Apollo, as this is an implicit field.
 * If we don't include this when using Apollo, the __typename information is
 * lost when we parse values and then serialize back to the js-values again
 */
let add_typename_to_selection_set: t;

/**
 * This is a GraphQL AST transform that removes the `__typename` field from
 * unions. The PPX will add this in the printer stage, so it is always there.
 * The PPX will not allow any scalar fields on a union except for the inline
 * fragments, so we make sure to remove it before doing any further processing.
 */
let remove_typename_from_union: t;

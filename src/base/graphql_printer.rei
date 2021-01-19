exception Cant_find_fragment_type(Source_pos.spanning(string));

type t =
  | String(string)
  | FragmentNameRef(string)
  | FragmentQueryRef(string);

let type_ref_name: Schema.type_ref => string;
let print_type: Graphql_ast.type_ref => string;
let print_document: (Schema.t, Graphql_ast.document) => array(t);

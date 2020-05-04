module Visitor: Traversal_utils.VisitorSig = {
  open Traversal_utils;
  open Graphql_ast;
  open Source_pos;

  include AbstractVisitor;

  type type_def = {
    name: string,
    required: bool,
  };
  type t = {
    types_: Hashtbl.t(string, option(type_def)),
    mutable argType: option(type_def),
    mutable nameSpan: option(spanning(string)),
  };
  let make_self = () => {
    types_: Hashtbl.create(0),
    argType: None,
    nameSpan: None,
  };
};

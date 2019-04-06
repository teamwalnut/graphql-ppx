module Visitor: Traversal_utils.VisitorSig = {
  open Traversal_utils;
  open Source_pos;
  open Graphql_ast;

  include AbstractVisitor;

  type t = Hashtbl.t(string, spanning(string));

  let make_self = () => Hashtbl.create(0);

  let enter_operation_definition = (self, _, def) => {
    let () = Hashtbl.clear(self);
    switch (def.item.o_variable_definitions) {
    | None => ()
    | Some({item, _}) =>
      List.iter(((name, _)) => Hashtbl.add(self, name.item, name), item)
    };
  };

  let enter_variable_value = (self, _, name) =>
    Hashtbl.remove(self, name.item);

  let exit_operation_definition = (self, ctx, def) =>
    Hashtbl.iter(
      (_, v) => {
        let message =
          switch (def.item.o_name) {
          | None => Printf.sprintf("Variable \"$%s\" is never used.", v.item)
          | Some(name) =>
            Printf.sprintf(
              "Variable \"$%s\" is never used in operation \"%s\"",
              v.item,
              name.item,
            )
          };

        Context.push_error(ctx, v.span, message);
      },
      self,
    );
};

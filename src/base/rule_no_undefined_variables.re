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

  let enter_variable_value = (self, ctx, def) =>
    if (!Hashtbl.mem(self, def.item)) {
      let message =
        Printf.sprintf(
          "Variable \"%s\" not found in operation. Make sure it's defined!",
          def.item,
        );

      Context.push_error(ctx, def.span, message);
    };
};

module Visitor: Traversal_utils.VisitorSig = {
  open Traversal_utils;
  open Source_pos;
  open Graphql_ast;

  include AbstractVisitor;

  type opts = {mutable active: bool};
  type t = (opts, Hashtbl.t(string, spanning(string)));

  let make_self = () => ({active: false}, Hashtbl.create(0));

  let enter_operation_definition = ((opts, self), _, def) => {
    opts.active = true;
    let () = Hashtbl.clear(self);
    switch (def.item.o_variable_definitions) {
    | None => ()
    | Some({item, _}) =>
      List.iter(((name, _)) => Hashtbl.add(self, name.item, name), item)
    };
  };

  let exit_operation_definition = ((opts, _), _, _) => {
    opts.active = false;
  };

  let enter_variable_value = ((opts, self), ctx, def) =>
    if (opts.active && !Hashtbl.mem(self, def.item)) {
      let message =
        Printf.sprintf(
          "Variable \"%s\" not found in operation. Make sure it's defined!",
          def.item,
        );

      Context.push_error(ctx, def.span, message);
    };
};

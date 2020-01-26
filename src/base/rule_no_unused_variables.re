module Visitor: Traversal_utils.VisitorSig = {
  open Traversal_utils;
  open Source_pos;
  open Graphql_ast;

  include AbstractVisitor;

  type t = Hashtbl.t(string, (source_position, source_position));

  let make_self = () => Hashtbl.create(0);

  let enter_fragment_definition = (self, _, def) => {
    let () = Hashtbl.clear(self);
    Result_decoder.getFragmentArgumentDefinitions(def.item.fg_directives)
    |> List.iter(((name, _, span, _)) => {Hashtbl.add(self, name, span)});
  };

  let exit_fragment_definition = (self, ctx, def) =>
    Hashtbl.iter(
      (v, span) => {
        let message =
          Printf.sprintf(
            "Variable \"$%s\" is never used in fragment \"%s\"",
            v,
            def.item.fg_name.item,
          );

        Context.push_error(ctx, span, message);
      },
      self,
    );

  let enter_operation_definition = (self, _, def) => {
    let () = Hashtbl.clear(self);
    switch (def.item.o_variable_definitions) {
    | None => ()
    | Some({item, _}) =>
      List.iter(
        ((name, _)) => Hashtbl.add(self, name.item, name.span),
        item,
      )
    };
  };

  let exit_operation_definition = (self, ctx, def) =>
    Hashtbl.iter(
      (v, span) => {
        let message =
          switch (def.item.o_name) {
          | None => Printf.sprintf("Variable \"$%s\" is never used.", v)
          | Some(name) =>
            Printf.sprintf(
              "Variable \"$%s\" is never used in operation \"%s\"",
              v,
              name.item,
            )
          };

        Context.push_error(ctx, span, message);
      },
      self,
    );

  let enter_variable_value = (self, _, name) =>
    Hashtbl.remove(self, name.item);
};

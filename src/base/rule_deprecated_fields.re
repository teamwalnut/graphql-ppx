module Visitor: Traversal_utils.VisitorSig = {
  open Traversal_utils;
  open Source_pos;
  open Graphql_ast;

  include AbstractVisitor;

  type t = unit;
  let make_self = () => ();

  let enter_field = (_, ctx, def) => {
    let field_meta =
      Context.parent_type(ctx)
      |> Option.flat_map(t => Schema.lookup_field(t, def.item.fd_name.item));

    field_meta
    |> Option.map((field: Schema.field_meta) => {
         switch (field.fm_deprecation_reason) {
         | None => ()
         | Some(reason) =>
           let message =
             Printf.sprintf(
               "Field \"%s\" has been deprecated. Reason: %s",
               field.fm_name,
               reason,
             );
           Context.push_warning(ctx, def.span, message);
         }
       })
    |> ignore;
  };
};

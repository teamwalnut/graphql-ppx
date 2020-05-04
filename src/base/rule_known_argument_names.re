module Visitor: Traversal_utils.VisitorSig = {
  open Traversal_utils;
  open Source_pos;
  open Graphql_ast;
  open Schema;

  type arg_pos =
    | Directive(string)
    | Field(string, string);

  type known_args = option((arg_pos, list(argument_meta)));

  type t = ref(known_args);
  include Traversal_utils.AbstractVisitor;

  let make_self = () => ref(None);

  let report_error = (ctx, span, pos, arg_name) => {
    let msg =
      switch (pos) {
      | Directive(dir_name) =>
        Printf.sprintf(
          "Unknown argument \"%s\" on directive \"%s\"",
          arg_name,
          dir_name,
        )
      | Field(field_name, type_name) =>
        Printf.sprintf(
          "Unknown argument \"%s\" on field \"%s\" of type \"%s\"",
          arg_name,
          field_name,
          type_name,
        )
      };

    Context.push_error(ctx, span, msg);
  };

  let enter_argument = (known_args, ctx, (name, _, _)) =>
    switch (known_args^) {
    | None => ()
    | Some((pos, known_args)) =>
      if ((!) @@ List.exists(am => am.am_name == name.item, known_args)) {
        report_error(ctx, name.span, pos, name.item);
      }
    };

  let enter_directive = (known_args, ctx, directive) =>
    known_args :=
      Schema.lookup_directive(ctx.schema, directive.item.d_name.item)
      |> Option.map(dm => (Directive(dm.dm_name), dm.dm_arguments));

  let exit_directive = (known_args, _, _) => known_args := None;

  let enter_field = (known_args, ctx, field) => {
    let field_name = field.item.fd_name.item;
    known_args :=
      (
        switch (Context.parent_type(ctx)) {
        | Some(parent_type) =>
          Schema.lookup_field(parent_type, field_name)
          |> Option.map(f =>
               (
                 Field(field_name, Schema.type_name(parent_type)),
                 f.fm_arguments,
               )
             )
        | None => None
        }
      );
  };

  let exit_field = (known_args, _, _) => known_args := None;
};

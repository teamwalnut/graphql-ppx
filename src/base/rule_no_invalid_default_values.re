module Visitor: Traversal_utils.VisitorSig = {
  open Traversal_utils;
  open Schema;
  open Source_pos;

  include AbstractVisitor;

  type t = {
    mutable argType: option(string),
    mutable nameSpan: option(span),
  };
  let make_self = () => {argType: None, nameSpan: None};

  let enter_argument =
      (
        self,
        ctx,
        (name: spanning(string), arg_type: option(type_ref), value),
      ) => {
    switch (
      arg_type
      |> Option.map(innermost_name)
      |> Option.flat_map(lookup_type(ctx.schema))
    ) {
    | Some(type_meta) when type_meta |> is_type_default =>
      self.argType = Some(type_meta |> type_name);
      self.nameSpan = Some(name.span);
    | Some(_)
    | None => ()
    };
  };

  let report_error = (ctx, expected, received, span) => {
    Context.push_error(
      ctx,
      span,
      Printf.sprintf(
        "Invalid default value. Expected \"%s\" but received \"%s\"",
        expected,
        received,
      ),
    );
  };

  let check_input = (ctx, expected, received, span) => {
    switch (expected) {
    | None => ()
    | Some(expected) when expected == received => ()
    | Some(expected) => report_error(ctx, expected, received, span)
    };
  };

  let enter_int_value = (self, ctx, value) => {
    check_input(ctx, self.argType, "Int", value.span);
  };
  let enter_float_value = (self, ctx, value) => {
    check_input(ctx, self.argType, "Float", value.span);
  };
  let enter_string_value = (self, ctx, value) => {
    switch (self.argType) {
    | Some("ID") => check_input(ctx, self.argType, "ID", value.span)
    | _ => check_input(ctx, self.argType, "String", value.span)
    };
  };
  let enter_bool_value = (self, ctx, value) => {
    check_input(ctx, self.argType, "Bool", value.span);
  };
  let enter_object_value = (self, ctx, value) => {
    check_input(ctx, self.argType, "Object", value.span);
    // We are done, as the above functions also apply to object fields
    self.argType = None;
    self.nameSpan = None;
  };
  let enter_list_value = (self, ctx, value) => {
    check_input(ctx, self.argType, "List", value.span);
    // We are done, as the above functions also apply to list object fields
    self.argType = None;
    self.nameSpan = None;
  };
  let enter_enum_value = (self, ctx, value) => {
    check_input(ctx, self.argType, "Enum", value.span);
    // We are done, as the above functions also apply to enum object fields
    self.argType = None;
    self.nameSpan = None;
  };
  let exit_argument = (self, _, _) => {
    self.argType = None;
    self.nameSpan = None;
  };
};

module Visitor: Traversal_utils.VisitorSig = {
  open Traversal_utils;
  open Graphql_ast;
  open Source_pos;
  open Type_utils.Generic;

  include AbstractVisitor;

  type t = {
    variable_types: Hashtbl.t(string, tree),
    mutable arg_type: option((spanning(string), tree)),
  };
  let make_self = () => {variable_types: Hashtbl.create(0), arg_type: None};

  let enter_operation_definition = (self, ctx, def) => {
    let () = Hashtbl.clear(self.variable_types);
    switch (def.item.o_variable_definitions) {
    | None => ()
    | Some({item, _}) =>
      List.iter(
        ((name, {vd_type: {item: type_}})) =>
          Hashtbl.add(
            self.variable_types,
            name.item,
            type_ |> from_graphql_ast_tr(~schema=ctx.schema),
          ),
        item,
      )
    };
  };

  let enter_argument =
      (
        self,
        ctx,
        (name: spanning(string), arg_type: option(Schema.type_ref), _value),
      ) => {
    self.arg_type =
      arg_type
      |> Option.map(from_schema_tr(~schema=ctx.schema))
      |> Option.map(tr => (name, tr));
  };

  let check_apply =
      (
        ctx,
        tr1: tree,
        tr2: tree,
        arg_name: spanning(string),
        name: spanning(string),
      ) =>
    switch (can_apply(tr1, tr2)) {
    | Ok => ()
    | Unequal =>
      Traversal_utils.Context.push_error(
        ctx,
        name.span,
        generate_error(
          MismatchedTypes(tr1 |> string_of_tree, tr2 |> string_of_tree),
        ),
      )
    | RequiredMismatch =>
      Traversal_utils.Context.push_error(
        ctx,
        name.span,
        generate_error(MismatchedRequiredVar(arg_name.item, name.item)),
      )
    };

  let enter_variable_value = (self, ctx, name) => {
    switch (
      self.arg_type,
      try(Some((name, Hashtbl.find(self.variable_types, name.item)))) {
      | Not_found => None
      },
    ) {
    | (None, _) => ()
    | (Some((arg_name, NonNull(_))), None) =>
      // TODO: This could be redundant now, check if rule_all_required_arguments.re is still necessary (or if it covers more cases like Fragments, etc.)
      Traversal_utils.Context.push_error(
        ctx,
        name.span,
        generate_error(RequiredVariableMissing(arg_name.item, name.item)),
      )
    | (Some((_, T(_))), None)
    | (Some((_, List(_))), None) => ()
    | (Some((arg_name, arg_type)), Some((_v_name, value_type))) =>
      check_apply(ctx, arg_type, value_type, arg_name, name)
    };
  };

  let check_pre_value = (~reset=false, generic, self, ctx, value) => {
    switch (self.arg_type) {
    | Some((arg_name, arg_type)) =>
      check_apply(
        ctx,
        arg_type,
        NonNull(T(generic)),
        arg_name,
        Source_pos.replace(value, ""),
      )
    | None => ()
    };
    if (reset) {
      self.arg_type = None;
    };
  };

  let enter_int_value = check_pre_value(Int);
  let enter_float_value = check_pre_value(Float);
  let enter_string_value = check_pre_value(String);
  let enter_bool_value = check_pre_value(Boolean);
  let enter_object_value = check_pre_value(~reset=true, InputObject);
  // TODO: For Enum, check enum fields from schema
  let enter_enum_value = check_pre_value(~reset=true, Enum);
  let enter_list_value = (self, ctx, value) => {
    switch (self.arg_type) {
    | Some((_, NonNull(List(_))))
    | Some((_, List(_)))
    | Some((_, NonNull(T(EmptyList))))
    | Some((_, T(EmptyList)))
    | None => ()
    | Some((_name, arg_type)) =>
      Traversal_utils.Context.push_error(
        ctx,
        value.span,
        generate_error(MismatchedTypes(arg_type |> string_of_tree, "List")),
      )
    };
    // We are done, as the above functions also apply to list object fields
    self.arg_type = None;
  };
  let exit_argument = (self, _, _) => {
    self.arg_type = None;
  };
};

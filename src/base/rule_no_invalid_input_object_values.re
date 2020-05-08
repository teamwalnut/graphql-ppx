module Visitor: Traversal_utils.VisitorSig = {
  open Traversal_utils;
  open Graphql_ast;
  open Source_pos;

  include AbstractVisitor;

  type t = {
    variable_types: Hashtbl.t(string, Type_utils.Generic.tree),
    arg_type: Hashtbl.t(string, Type_utils.Generic.tree),
    mutable name_span: option(spanning(string)),
  };
  let make_self = () => {
    variable_types: Hashtbl.create(0),
    arg_type: Hashtbl.create(0),
    name_span: None,
  };

  let enter_operation_definition = (self, ctx, def) => {
    let () = Hashtbl.clear(self.variable_types);
    switch (def.item.o_variable_definitions) {
    | None => ()
    | Some({item, _}) =>
      List.iter(
        ((name, {vd_type: {item as type_}})) =>
          Hashtbl.add(
            self.variable_types,
            name.item,
            type_
            |> Type_utils.Generic.from_graphql_ast_tr(~schema=ctx.schema),
          ),
        item,
      )
    };
  };

  let enter_argument =
      (
        self,
        ctx,
        (
          name: spanning(string),
          arg_type: option(Schema.type_ref),
          value: spanning(input_value),
        ),
      ) => {
    switch (
      arg_type
      |> Option.map(Schema.innermost_name)
      |> Option.flat_map(Schema.lookup_type(ctx.schema))
    ) {
    | Some(InputObject(meta)) =>
      Hashtbl.clear(self.arg_type);

      meta.iom_input_fields
      |> List.iter(({Schema.am_arg_type, am_name}) =>
           Hashtbl.add(
             self.arg_type,
             am_name,
             am_arg_type
             |> Type_utils.Generic.from_schema_tr(~schema=ctx.schema),
           )
         );
      self.name_span = Some(name);
    | Some(_) => ()
    | None => ()
    };
  };

  let enter_object_value =
      (
        self,
        ctx,
        value: spanning(list((spanning(string), spanning(input_value)))),
      ) => {
    let arg_name =
      self.name_span
      |> Option.get_or_else({item: "__arg__", span: Source_pos.nospan});
    Type_utils.Generic.(
      Hashtbl.iter(
        (key, value_type) => {
          switch (
            value_type,
            value.item
            |> List.find_opt(((name, value)) => {name.item == key})
            |> Option.map(((name, value)) =>
                 (
                   switch (value.item) {
                   | Iv_variable(val_name) =>
                     Source_pos.replace(name, val_name)
                   | _ => name
                   },
                   Source_pos.replace(
                     value,
                     value.item
                     |> from_graphql_ast_iv(
                          ~arguments=self.variable_types,
                          ~schema=ctx.schema,
                        ),
                   ),
                 )
               ),
          ) {
          | (NonNull(_), None) =>
            Traversal_utils.Context.push_error(
              ctx,
              arg_name.span,
              generate_error(RequiredFieldMissing(arg_name.item, key)),
            )
          | (T(_), None)
          | (List(_), None) => ()
          | (key_type, Some((name, value))) =>
            switch (can_apply(key_type, value.item)) {
            | Ok => ()
            | Unequal =>
              Traversal_utils.Context.push_error(
                ctx,
                name.span,
                generate_error(
                  MismatchedTypes(
                    key_type |> string_of_tree,
                    value.item |> string_of_tree,
                  ),
                ),
              )
            | RequiredMismatch =>
              Traversal_utils.Context.push_error(
                ctx,
                name.span,
                generate_error(MismatchedRequiredVar(key, name.item)),
              )
            }
          }
        },
        self.arg_type,
      )
    );
  };

  let exit_object_value = (self, _, _) => {
    // We are done, as the above functions also apply to object fields
    Hashtbl.clear(self.arg_type);
    self.name_span = None;
  };

  let exit_argument = (self, _, _) => {
    Hashtbl.clear(self.arg_type);
    self.name_span = None;
  };
};

module Visitor: Traversal_utils.VisitorSig = {
  open Traversal_utils;
  open Graphql_ast;
  open Source_pos;
  open Type_utils.Generic;

  include AbstractVisitor;

  type t = {variable_types: Hashtbl.t(string, tree)};
  let make_self = () => {variable_types: Hashtbl.create(0)};

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
            type_ |> from_graphql_ast_tr(~schema=ctx.schema),
          ),
        item,
      )
    };
  };

  let rec parse_input_object =
          (
            self,
            ctx,
            top_pos: spanning(string),
            type_: Schema.type_ref,
            value: spanning(input_value),
          ) => {
    switch (
      type_ |> Schema.innermost_name |> Schema.lookup_type(ctx.schema),
      value.item,
    ) {
    | (
        Some(InputObject(obj_meta)),
        Iv_list([{item: Iv_object(value), _}, ..._]),
      )
    // | (
    //     Some(InputObject(obj_meta)),
    //     Iv_list([{item: Iv_object(value), _}, ..._]),
    //   )
    | (Some(InputObject(obj_meta)), Iv_object(value)) =>
      obj_meta.iom_input_fields
      |> List.iter(({Schema.am_arg_type, am_name}) => {
           let value_field =
             value
             |> List.find_opt(((name, value)) => {name.item == am_name});

           switch (value_field) {
           | Some(value_field) =>
             parse_input_object(
               self,
               ctx,
               fst(value_field),
               am_arg_type,
               snd(value_field),
             )
           | None => ()
           };

           switch (
             am_arg_type |> from_schema_tr(~schema=ctx.schema),
             value_field
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
               top_pos.span,
               generate_error(RequiredFieldMissing(top_pos.item, am_name)),
             )
           | (T(_), None)
           | (List(_), None) => ()
           | (key_type, Some((name, value))) =>
             switch (can_apply(key_type, value.item)) {
             | Ok => ()
             | Unequal =>
               Traversal_utils.Context.push_error(
                 ctx,
                 value.span,
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
                 generate_error(MismatchedRequiredVar(am_name, name.item)),
               )
             }
           };
         })
    | _ => ()
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
    switch (arg_type |> Option.map(from_schema_tr(~schema=ctx.schema))) {
    | Some(T(InputObject))
    | Some(NonNull(T(InputObject)))
    | Some(List(T(InputObject)))
    | Some(NonNull(List(T(InputObject)))) =>
      parse_input_object(
        self,
        ctx,
        name,
        arg_type |> Option.unsafe_unwrap,
        value,
      )
    | _ => ()
    };
  };
};

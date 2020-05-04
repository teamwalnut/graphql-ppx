module Visitor: Traversal_utils.VisitorSig = {
  open Traversal_utils;
  open Graphql_ast;
  open Source_pos;

  include AbstractVisitor;

  type type_def = {
    name: string,
    required: bool,
  };
  type t = {
    types_: Hashtbl.t(string, option(type_def)),
    mutable argType: option(type_def),
    mutable nameSpan: option(spanning(string)),
  };
  let make_self = () => {
    types_: Hashtbl.create(0),
    argType: None,
    nameSpan: None,
  };

  let toGenericTypeName = (ty: Schema.type_meta) =>
    switch (ty) {
    | Scalar(_) when ty |> Schema.is_type_default => ty |> Schema.type_name
    | Scalar(_) => "CustomScalar"
    | Object(_)
    | InputObject(_) => "Object"
    | Enum(_) => "Enum"
    | Interface(_) => "Interface"
    | Union(_) => "Union"
    };

  type error =
    | MismatchedTypes(string, string)
    | MismatchedRequired(string, string);
  let report_error = (ctx, span, error: error) => {
    let msg =
      switch (error) {
      | MismatchedTypes(expected, received) =>
        Printf.sprintf(
          "Invalid argument. Expected \"%s\" but received \"%s\".",
          expected,
          received,
        )
      | MismatchedRequired(arg_name, val_name) =>
        Printf.sprintf(
          "Invalid argument. Trying to apply the nullable variable \"$%s\" to the required argument \"%s\".",
          val_name,
          arg_name,
        )
      };
    Context.push_error(ctx, span, msg);
  };

  let enter_operation_definition = (self, ctx, def) => {
    let () = Hashtbl.clear(self.types_);
    switch (def.item.o_variable_definitions) {
    | None => ()
    | Some({item, _}) =>
      List.iter(
        ((name, {vd_type: {item as type_}})) =>
          Hashtbl.add(
            self.types_,
            name.item,
            type_
            |> innermost_name
            |> Schema.lookup_type(ctx.schema)
            |> Option.map(toGenericTypeName)
            |> Option.map(name =>
                 {
                   name,
                   required:
                     switch (type_) {
                     | Tr_non_null_named(_)
                     | Tr_non_null_list(_) => true
                     | Tr_named(_)
                     | Tr_list(_) => false
                     },
                 }
               ),
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
    | Some(type_meta) =>
      self.argType =
        Some({
          name: type_meta |> toGenericTypeName,
          required:
            switch (arg_type |> Option.unsafe_unwrap) {
            | NonNull(_) => true
            | Named(_)
            | List(_) => false
            },
        });
      self.nameSpan = Some(name);
    | None => ()
    };
  };

  let enter_variable_value = (self, ctx, name) => {
    let report = report_error(ctx, name.span);

    switch (
      self.argType,
      try(Hashtbl.find(self.types_, name.item)) {
      | Not_found => None
      },
    ) {
    | (None, _)
    | (_, None) => ()
    | (Some({name as argType}), Some({name: varType}))
        when
          (
            switch (argType, varType) {
            | ("ID", "String")
            | ("String", "ID") => false
            | _ => true
            }
          )
          && argType != "CustomScalar"
          && argType != varType =>
      report(MismatchedTypes(argType, varType))
    | (Some({required: true}), Some({required: false})) =>
      report(
        MismatchedRequired(
          (self.nameSpan |> Option.unsafe_unwrap).item,
          name.item,
        ),
      )
    | (Some({name: "CustomScalar"}), Some(_)) => ()
    | _ => ()
    };
  };

  let enter_object_value = (self, ctx, value) => {
    // We are done, as the above functions also apply to object fields
    self.argType = None;
    self.nameSpan = None;
  };

  let exit_argument = (self, _, _) => {
    self.argType = None;
    self.nameSpan = None;
  };
};

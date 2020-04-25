open Graphql_ast;
open Source_pos;

type ctx = {
  map_loc: ((source_position, source_position)) => Result_structure.loc,
  fragments: Hashtbl.t(string, Graphql_ast.fragment),
  schema: Schema.t,
  errors: ref(list((Result_structure.loc, string))),
  type_stack: list(option(Schema.type_meta)),
  type_literal_stack: list(option(Schema.type_ref)),
  input_type_stack: list(option(Schema.type_meta)),
  input_type_literal_stack: list(option(Schema.type_ref)),
  parent_type_stack: list(option(Schema.type_meta)),
};

module type VisitorSig = {
  type t;

  let make_self: unit => t;

  let enter_document: (t, ctx, document) => unit;
  let exit_document: (t, ctx, document) => unit;

  let enter_operation_definition: (t, ctx, spanning(operation)) => unit;
  let exit_operation_definition: (t, ctx, spanning(operation)) => unit;

  let enter_fragment_definition: (t, ctx, spanning(fragment)) => unit;
  let exit_fragment_definition: (t, ctx, spanning(fragment)) => unit;

  let enter_variable_definition:
    (t, ctx, (spanning(string), variable_definition)) => unit;
  let exit_variable_definition:
    (t, ctx, (spanning(string), variable_definition)) => unit;

  let enter_directive: (t, ctx, spanning(directive)) => unit;
  let exit_directive: (t, ctx, spanning(directive)) => unit;

  let enter_argument:
    (t, ctx, (spanning(string), spanning(input_value))) => unit;
  let exit_argument:
    (t, ctx, (spanning(string), spanning(input_value))) => unit;

  let enter_selection_set: (t, ctx, spanning(list(selection))) => unit;
  let exit_selection_set: (t, ctx, spanning(list(selection))) => unit;

  let enter_field: (t, ctx, spanning(field)) => unit;
  let exit_field: (t, ctx, spanning(field)) => unit;

  let enter_fragment_spread: (t, ctx, spanning(fragment_spread)) => unit;
  let exit_fragment_spread: (t, ctx, spanning(fragment_spread)) => unit;

  let enter_inline_fragment: (t, ctx, spanning(inline_fragment)) => unit;
  let exit_inline_fragment: (t, ctx, spanning(inline_fragment)) => unit;

  let enter_null_value: (t, ctx, spanning(unit)) => unit;
  let exit_null_value: (t, ctx, spanning(unit)) => unit;

  let enter_int_value: (t, ctx, spanning(int)) => unit;
  let exit_int_value: (t, ctx, spanning(int)) => unit;

  let enter_float_value: (t, ctx, spanning(float)) => unit;
  let exit_float_value: (t, ctx, spanning(float)) => unit;

  let enter_string_value: (t, ctx, spanning(string)) => unit;
  let exit_string_value: (t, ctx, spanning(string)) => unit;

  let enter_bool_value: (t, ctx, spanning(bool)) => unit;
  let exit_bool_value: (t, ctx, spanning(bool)) => unit;

  let enter_enum_value: (t, ctx, spanning(string)) => unit;
  let exit_enum_value: (t, ctx, spanning(string)) => unit;

  let enter_variable_value: (t, ctx, spanning(string)) => unit;
  let exit_variable_value: (t, ctx, spanning(string)) => unit;

  let enter_list_value: (t, ctx, list(spanning(input_value))) => unit;
  let exit_list_value: (t, ctx, list(spanning(input_value))) => unit;

  let enter_object_value:
    (t, ctx, list((spanning(string), spanning(input_value)))) => unit;
  let exit_object_value:
    (t, ctx, list((spanning(string), spanning(input_value)))) => unit;

  let enter_object_field:
    (t, ctx, (spanning(string), spanning(input_value))) => unit;
  let exit_object_field:
    (t, ctx, (spanning(string), spanning(input_value))) => unit;
};

module AbstractVisitor = {
  let enter_document = (_self, _ctx, _) => ();
  let exit_document = (_self, _ctx, _) => ();

  let enter_operation_definition = (_self, _ctx, _) => ();
  let exit_operation_definition = (_self, _ctx, _) => ();

  let enter_fragment_definition = (_self, _ctx, _) => ();
  let exit_fragment_definition = (_self, _ctx, _) => ();

  let enter_variable_definition = (_self, _ctx, _) => ();
  let exit_variable_definition = (_self, _ctx, _) => ();

  let enter_directive = (_self, _ctx, _) => ();
  let exit_directive = (_self, _ctx, _) => ();

  let enter_argument = (_self, _ctx, _) => ();
  let exit_argument = (_self, _ctx, _) => ();

  let enter_selection_set = (_self, _ctx, _) => ();
  let exit_selection_set = (_self, _ctx, _) => ();

  let enter_field = (_self, _ctx, _) => ();
  let exit_field = (_self, _ctx, _) => ();

  let enter_fragment_spread = (_self, _ctx, _) => ();
  let exit_fragment_spread = (_self, _ctx, _) => ();

  let enter_inline_fragment = (_self, _ctx, _) => ();
  let exit_inline_fragment = (_self, _ctx, _) => ();

  let enter_null_value = (_self, _ctx, _) => ();
  let exit_null_value = (_self, _ctx, _) => ();

  let enter_int_value = (_self, _ctx, _) => ();
  let exit_int_value = (_self, _ctx, _) => ();

  let enter_float_value = (_self, _ctx, _) => ();
  let exit_float_value = (_self, _ctx, _) => ();

  let enter_string_value = (_self, _ctx, _) => ();
  let exit_string_value = (_self, _ctx, _) => ();

  let enter_bool_value = (_self, _ctx, _) => ();
  let exit_bool_value = (_self, _ctx, _) => ();

  let enter_enum_value = (_self, _ctx, _) => ();
  let exit_enum_value = (_self, _ctx, _) => ();

  let enter_variable_value = (_self, _ctx, _) => ();
  let exit_variable_value = (_self, _ctx, _) => ();

  let enter_list_value = (_self, _ctx, _) => ();
  let exit_list_value = (_self, _ctx, _) => ();

  let enter_object_value = (_self, _ctx, _) => ();
  let exit_object_value = (_self, _ctx, _) => ();

  let enter_object_field = (_self, _ctx, _) => ();
  let exit_object_field = (_self, _ctx, _) => ();
};

module Context = {
  let push_type = (ctx, type_ref) => {
    ...ctx,
    type_stack: [
      Option.flat_map(
        type_ref =>
          Schema.lookup_type(ctx.schema, Schema.innermost_name(type_ref)),
        type_ref,
      ),
      ...ctx.type_stack,
    ],
    type_literal_stack: [type_ref, ...ctx.type_literal_stack],
  };
  let push_input_type = (ctx, type_ref) => {
    ...ctx,
    input_type_stack: [
      Option.flat_map(
        type_ref =>
          Schema.lookup_type(ctx.schema, Schema.innermost_name(type_ref)),
        type_ref,
      ),
      ...ctx.input_type_stack,
    ],
    input_type_literal_stack: [type_ref, ...ctx.input_type_literal_stack],
  };
  let push_parent_type = ctx => {
    let top =
      switch (ctx.type_stack) {
      | [t, ..._] => t
      | [] => None
      };
    {...ctx, parent_type_stack: [top, ...ctx.parent_type_stack]};
  };
  let parent_type = ctx =>
    switch (ctx.parent_type_stack) {
    | [t, ..._] => t
    | [] => None
    };
  let current_input_type_literal = ctx =>
    switch (ctx.input_type_literal_stack) {
    | [t, ..._] => t
    | [] => None
    };
  let push_error = (ctx, loc, msg) =>
    ctx.errors := [(ctx.map_loc(loc), msg), ...ctx.errors^];
};

let rec as_schema_type_ref =
  fun
  | Tr_named({item, _}) => Schema.Named(item)
  | Tr_list({item, _}) => Schema.List(as_schema_type_ref(item))
  | Tr_non_null_named({item, _}) => Schema.NonNull(Schema.Named(item))
  | Tr_non_null_list({item, _}) =>
    Schema.NonNull(Schema.List(as_schema_type_ref(item)));

module Visitor = (V: VisitorSig) => {
  let enter_input_value = (self, ctx, value) =>
    switch (value.item) {
    | Iv_null => V.enter_null_value(self, ctx, Source_pos.replace(value, ()))
    | Iv_int(i) => V.enter_int_value(self, ctx, Source_pos.replace(value, i))
    | Iv_float(f) =>
      V.enter_float_value(self, ctx, Source_pos.replace(value, f))
    | Iv_string(s) =>
      V.enter_string_value(self, ctx, Source_pos.replace(value, s))
    | Iv_boolean(b) =>
      V.enter_bool_value(self, ctx, Source_pos.replace(value, b))
    | Iv_enum(e) =>
      V.enter_enum_value(self, ctx, Source_pos.replace(value, e))
    | Iv_variable(v) =>
      V.enter_variable_value(self, ctx, Source_pos.replace(value, v))
    | Iv_list(l) => V.enter_list_value(self, ctx, l)
    | Iv_object(o) => V.enter_object_value(self, ctx, o)
    };

  let exit_input_value = (self, ctx, value) =>
    switch (value.item) {
    | Iv_null => V.exit_null_value(self, ctx, Source_pos.replace(value, ()))
    | Iv_int(i) => V.exit_int_value(self, ctx, Source_pos.replace(value, i))
    | Iv_float(f) =>
      V.exit_float_value(self, ctx, Source_pos.replace(value, f))
    | Iv_string(s) =>
      V.exit_string_value(self, ctx, Source_pos.replace(value, s))
    | Iv_boolean(b) =>
      V.exit_bool_value(self, ctx, Source_pos.replace(value, b))
    | Iv_enum(e) =>
      V.exit_enum_value(self, ctx, Source_pos.replace(value, e))
    | Iv_variable(v) =>
      V.exit_variable_value(self, ctx, Source_pos.replace(value, v))
    | Iv_list(l) => V.exit_list_value(self, ctx, l)
    | Iv_object(o) => V.exit_object_value(self, ctx, o)
    };

  let rec visit_input_value = (self, ctx, value) => {
    let () = enter_input_value(self, ctx, value);
    let () =
      switch (value.item) {
      | Iv_object(fields) =>
        List.iter(
          ((key, value)) => {
            let inner_type =
              Context.current_input_type_literal(ctx)
              |> Option.flat_map(
                   fun
                   | Schema.Named(name)
                   | Schema.NonNull(Schema.Named(name)) =>
                     Schema.lookup_type(ctx.schema, name)
                   | _ => None,
                 )
              |> Option.flat_map(t => Schema.lookup_input_field(t, key.item))
              |> Option.map(am => am.Schema.am_arg_type);
            let ctx = Context.push_input_type(ctx, inner_type);
            let () = V.enter_object_field(self, ctx, (key, value));
            let () = visit_input_value(self, ctx, value);
            V.exit_object_field(self, ctx, (key, value));
          },
          fields,
        )
      | Iv_list(items) =>
        let inner_type =
          Context.current_input_type_literal(ctx)
          |> Option.flat_map(
               fun
               | Schema.List(inner)
               | Schema.NonNull(Schema.List(inner)) => Some(inner)
               | _ => None,
             );
        let ctx = Context.push_input_type(ctx, inner_type);
        List.iter(visit_input_value(self, ctx), items);
      | _ => ()
      };
    exit_input_value(self, ctx, value);
  };

  let rec visit_inline_fragment = (self, ctx, inline_fragment) => {
    let ctx =
      switch (inline_fragment.item.if_type_condition) {
      | None => ctx
      | Some({item, _}) =>
        Context.push_type(ctx, Some(Schema.NonNull(Schema.Named(item))))
      };

    let () = V.enter_inline_fragment(self, ctx, inline_fragment);
    let () = visit_directives(self, ctx, inline_fragment.item.if_directives);
    let () =
      visit_selection_set(self, ctx, inline_fragment.item.if_selection_set);
    V.exit_inline_fragment(self, ctx, inline_fragment);
  }
  and visit_fragment_spread = (self, ctx, fragment_spread) => {
    let () = V.enter_fragment_spread(self, ctx, fragment_spread);
    let () = visit_directives(self, ctx, fragment_spread.item.fs_directives);
    V.exit_fragment_spread(self, ctx, fragment_spread);
  }
  and visit_field = (self, ctx, field) => {
    let meta_field =
      Context.parent_type(ctx)
      |> Option.flat_map(t => Schema.lookup_field(t, field.item.fd_name.item));
    let field_type = Option.map(fm => fm.Schema.fm_field_type, meta_field);
    let field_args = Option.map(fm => fm.Schema.fm_arguments, meta_field);
    let ctx = Context.push_type(ctx, field_type);
    let () = V.enter_field(self, ctx, field);
    let () = visit_arguments(self, ctx, field_args, field.item.fd_arguments);
    let () = visit_directives(self, ctx, field.item.fd_directives);
    let () =
      switch (field.item.fd_selection_set) {
      | None => ()
      | Some(selection_set) => visit_selection_set(self, ctx, selection_set)
      };
    V.exit_field(self, ctx, field);
  }
  and visit_selection = (self, ctx) =>
    fun
    | Field(field) => visit_field(self, ctx, field)
    | FragmentSpread(fragment_spread) =>
      visit_fragment_spread(self, ctx, fragment_spread)
    | InlineFragment(inline_fragment) =>
      visit_inline_fragment(self, ctx, inline_fragment)
  and visit_selection_set = (self, ctx, selection_set) => {
    let ctx = Context.push_parent_type(ctx);
    let () = V.enter_selection_set(self, ctx, selection_set);
    let () = List.iter(visit_selection(self, ctx), selection_set.item);
    V.exit_selection_set(self, ctx, selection_set);
  }
  and visit_arguments = (self, ctx, meta_args) =>
    fun
    | None => ()
    | Some({item, _}) =>
      List.iter(
        ((name, value)) => {
          let arg_type =
            meta_args
            |> Option.flat_map(meta_args =>
                 switch (
                   List.find(am => am.Schema.am_name == name.item, meta_args)
                 ) {
                 | am => Some(am)
                 | exception Not_found => None
                 }
               )
            |> Option.map(am => am.Schema.am_arg_type);
          let ctx = Context.push_input_type(ctx, arg_type);
          let () = V.enter_argument(self, ctx, (name, value));
          let () = visit_input_value(self, ctx, value);
          V.exit_argument(self, ctx, (name, value));
        },
        item,
      )
  and visit_directives = (self, ctx) =>
    List.iter(directive => {
      let meta_args =
        Schema.lookup_directive(ctx.schema, directive.item.d_name.item)
        |> Option.map(d => d.Schema.dm_arguments);
      let () = V.enter_directive(self, ctx, directive);
      let () =
        visit_arguments(self, ctx, meta_args, directive.item.d_arguments);
      V.exit_directive(self, ctx, directive);
    });

  let visit_variable_definitions = (self, ctx) =>
    fun
    | None => ()
    | Some({item, _}) =>
      List.iter(
        ((name, def)) => {
          let ctx =
            Context.push_input_type(
              ctx,
              Some(as_schema_type_ref(def.vd_type.item)),
            );
          let () = V.enter_variable_definition(self, ctx, (name, def));
          let () =
            switch (def.vd_default_value) {
            | None => ()
            | Some(value) => visit_input_value(self, ctx, value)
            };
          V.exit_variable_definition(self, ctx, (name, def));
        },
        item,
      );

  let visit_operation_definition = (self, ctx, operation) => {
    let () =
      visit_variable_definitions(self, ctx, operation.o_variable_definitions);
    let () = visit_directives(self, ctx, operation.o_directives);
    visit_selection_set(self, ctx, operation.o_selection_set);
  };

  let visit_fragment_definition = (self, ctx, fragment) => {
    let () = visit_directives(self, ctx, fragment.fg_directives);
    visit_selection_set(self, ctx, fragment.fg_selection_set);
  };

  let visit_definition = (self, ctx, def) => {
    let def_type =
      Schema.NonNull(
        Schema.Named(
          switch (def) {
          | Operation({item: {o_type: Query, _}, _}) =>
            Schema.query_type(ctx.schema) |> Schema.type_name
          | Operation({item: {o_type: Mutation, _}, _}) =>
            Schema.mutation_type(ctx.schema)
            |> Option.unsafe_unwrap
            |> Schema.type_name
          | Operation({item: {o_type: Subscription, _}, _}) =>
            Schema.subscription_type(ctx.schema)
            |> Option.unsafe_unwrap
            |> Schema.type_name
          | Fragment({item: {fg_type_condition: {item, _}, _}, _}) => item
          },
        ),
      );

    let ctx = Context.push_type(ctx, Some(def_type));
    switch (def) {
    | Operation(operation) =>
      let () = V.enter_operation_definition(self, ctx, operation);
      let () = visit_operation_definition(self, ctx, operation.item);
      V.exit_operation_definition(self, ctx, operation);
    | Fragment(fragment) =>
      let () = V.enter_fragment_definition(self, ctx, fragment);
      let () = visit_fragment_definition(self, ctx, fragment.item);
      V.exit_fragment_definition(self, ctx, fragment);
    };
  };

  let visit_document = (ctx, doc) => {
    let self = V.make_self();
    let () = V.enter_document(self, ctx, doc);
    let () = List.iter(visit_definition(self, ctx), doc);
    let () = V.exit_document(self, ctx, doc);
    self;
  };
};

let find_fragments = doc => {
  open Graphql_ast;
  open Source_pos;
  let lookup = Hashtbl.create(1);
  let () =
    List.iter(
      fun
      | Fragment(fragment) =>
        Hashtbl.add(lookup, fragment.item.fg_name.item, fragment.item)
      | _ => (),
      doc,
    );
  lookup;
};

let make_context = (config, document) => {
  map_loc: config.Generator_utils.map_loc,
  fragments: find_fragments(document),
  schema: config.Generator_utils.schema,
  errors: ref([]),
  type_stack: [],
  type_literal_stack: [],
  input_type_stack: [],
  input_type_literal_stack: [],
  parent_type_stack: [],
};

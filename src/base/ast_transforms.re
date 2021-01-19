type t = (Schema.t, Graphql_ast.document) => Graphql_ast.document;
type parent =
  | Definition(Graphql_ast.definition)
  | Selection(Graphql_ast.selection);

let get_parent_span =
  fun
  | Selection(Graphql_ast.Field({span})) => span
  | Selection(Graphql_ast.InlineFragment({span})) => span
  | Selection(Graphql_ast.FragmentSpread({span})) => span
  | Definition(Graphql_ast.Operation({span})) => span
  | Definition(Graphql_ast.Fragment({span})) => span;

// get's the type of a field name
let safe_get_field_type = (schema, ty: Schema.type_meta, name) => {
  let ty_fields =
    switch (ty) {
    | Interface({im_fields, _}) => im_fields
    | Object({om_fields, _}) => om_fields
    | _ => []
    };
  Schema.(
    List.find_opt(fm => fm.Schema.fm_name == name, ty_fields)
    |> Option.map(field => field.fm_field_type)
    |> Option.map(Graphql_printer.type_ref_name)
    |> Option.flat_map(Schema.lookup_type(schema))
  );
};

let traverse_selection_set = (schema, ty, selection_set, fn) => {
  Graphql_ast.(
    selection_set
    |> List.map(
         fun
         | Graphql_ast.InlineFragment(
             {
               item: {
                 if_type_condition: Some(type_condition),
                 if_selection_set: selection,
               },
             } as field,
           ) as parent => {
             let field_ty = Schema.lookup_type(schema, type_condition.item);
             switch (field_ty) {
             | None => parent
             | Some(field_ty) =>
               let selection_set =
                 fn(Selection(parent), schema, field_ty, selection.item);
               Graphql_ast.InlineFragment({
                 ...field,
                 item: {
                   ...field.item,
                   if_selection_set: {
                     ...selection,
                     item: selection_set,
                   },
                 },
               });
             };
           }
         | Graphql_ast.Field(
             {item: {fd_selection_set: Some(selection)}} as field,
           ) as parent => {
             let field_ty =
               safe_get_field_type(schema, ty, field.item.fd_name.item);
             switch (field_ty) {
             | None => parent
             | Some(field_ty) =>
               let selection_set =
                 fn(Selection(parent), schema, field_ty, selection.item);

               Graphql_ast.Field({
                 ...field,
                 item: {
                   ...field.item,
                   fd_selection_set:
                     Some({...selection, item: selection_set}),
                 },
               });
             };
           }
         | other => other,
       )
  );
};

let rec do_add_typename_to_selection_set = (parent, schema, ty, selection_set) => {
  let add_typename =
    switch (ty, selection_set) {
    | (_, []) => false
    // do not add typename to a single fragment spread. It will be added in the
    // fragment anyway, and it will prevent the result to be a single record of
    // the type of the fragment
    | (_, [Graphql_ast.FragmentSpread(_)]) => false
    // do not inject `__typename` in interfaces and unions, the printer does that
    // already behind the scenes
    | (Schema.Interface(_), _selection_set) => false
    | (Schema.Union(_), _selection_set) => false
    | (Schema.Object({om_name}), _selection_set) =>
      // do not inject __typename in top-level types
      Schema.(
        switch (schema) {
        | {meta: {sm_subscription_type: Some(name)}} when name == om_name =>
          false
        | {meta: {sm_mutation_type: Some(name)}} when name == om_name =>
          false
        | {meta: {sm_query_type: name}} when name == om_name => false
        | _ =>
          switch (parent) {
          // we cannot add typename when using @bsvariant
          | Selection(
              Graphql_ast.Field({item: {fd_directives: directives}}),
            ) =>
            !(
              directives
              |> List.exists((d: Source_pos.spanning(Graphql_ast.directive)) =>
                   d.item.d_name.item == "bsVariant"
                 )
            )
          | _ => true
          }
        }
      )
    | (_, _selection_set) => false
    };
  let selection_set =
    traverse_selection_set(
      schema,
      ty,
      selection_set,
      do_add_typename_to_selection_set,
    );

  let already_has_typename =
    selection_set
    |> List.exists(
         fun
         | Graphql_ast.Field({item: {fd_name: {item: "__typename"}}}) =>
           true
         | _ => false,
       );

  let parent_span = get_parent_span(parent);
  if (add_typename && !already_has_typename) {
    [
      Graphql_ast.Field({
        span: parent_span,
        item: {
          fd_alias: None,
          fd_name: {
            span: (
              {Source_pos.index: (-1), line: (-1), col: (-1)},
              {Source_pos.index: (-1), line: (-1), col: (-1)},
            ),
            item: "__typename",
          },
          fd_arguments: None,
          fd_directives: [],
          fd_selection_set: None,
        },
      }),
      ...selection_set,
    ];
  } else {
    selection_set;
  };
};

let rec do_remove_typename_from_union = (_parent, schema, ty, selection_set) => {
  let selection_set =
    switch (ty) {
    | Schema.Union(_) =>
      Graphql_ast.(
        selection_set
        |> List.fold_left(
             acc =>
               fun
               | Field({item: {fd_name: {item: "__typename"}}}) => acc
               | other => [other, ...acc],
             [],
           )
        |> List.rev
      )
    | _ => selection_set
    };
  traverse_selection_set(
    schema,
    ty,
    selection_set,
    do_remove_typename_from_union,
  );
};

let traverse_document_selections = (fn, schema: Schema.t, definitions) => {
  Graphql_ast.(
    definitions
    |> List.map(def => {
         switch (def) {
         | Operation({item: op, span}) as parent =>
           let ty_name =
             switch (op.o_type) {
             | Query => schema.meta.sm_query_type
             | Mutation =>
               Option.unsafe_unwrap(
                 ~reason="Cannot find mutation type",
                 schema.meta.sm_mutation_type,
               )
             | Subscription =>
               Option.unsafe_unwrap(
                 ~reason="Cannot find subscription type",
                 schema.meta.sm_subscription_type,
               )
             };
           let ty = Schema.lookup_type(schema, ty_name);
           switch (ty) {
           | None => parent
           | Some(ty) =>
             Operation({
               span,
               item: {
                 ...op,
                 o_selection_set: {
                   item:
                     fn(
                       Definition(parent),
                       schema,
                       ty,
                       op.o_selection_set.item,
                     ),
                   span: op.o_selection_set.span,
                 },
               },
             })
           };

         | Fragment({item: f, span}) as parent =>
           let ty_name = f.fg_type_condition.item;
           let ty = Schema.lookup_type(schema, ty_name);
           switch (ty) {
           | Some(ty) =>
             Fragment({
               item: {
                 ...f,
                 fg_selection_set: {
                   item:
                     fn(
                       Definition(parent),
                       schema,
                       ty,
                       f.fg_selection_set.item,
                     ),
                   span: f.fg_selection_set.span,
                 },
               },
               span,
             })
           | None => parent
           };
         }
       })
  );
};

let add_typename_to_selection_set =
  traverse_document_selections(do_add_typename_to_selection_set);
let remove_typename_from_union =
  traverse_document_selections(do_remove_typename_from_union);

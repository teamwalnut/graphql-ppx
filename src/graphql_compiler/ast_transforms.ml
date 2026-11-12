type t = Schema.t -> Graphql_ast.document -> Graphql_ast.document

type parent =
  | Definition of Graphql_ast.definition
  | Selection of Graphql_ast.selection

let get_parent_span = function
  | Selection (Graphql_ast.Field { span }) -> span
  | Selection (Graphql_ast.InlineFragment { span }) -> span
  | Selection (Graphql_ast.FragmentSpread { span }) -> span
  | Definition (Graphql_ast.Operation { span }) -> span
  | Definition (Graphql_ast.Fragment { span }) -> span

let safe_get_field_type schema (ty : Schema.type_meta) name =
  let ty_fields =
    match ty with
    | Interface { im_fields; _ } -> im_fields
    | Object { om_fields; _ } -> om_fields
    | _ -> []
  in
  let open Schema in
  List.find_opt (fun fm -> fm.Schema.fm_name = name) ty_fields
  |> Option.map (fun field -> field.fm_field_type)
  |> Option.map Graphql_printer.type_ref_name
  |> Option.flat_map (Schema.lookup_type schema)

let traverse_selection_set schema ty selection_set fn =
  let open Graphql_ast in
  selection_set
  |> List.map (function
       | Graphql_ast.InlineFragment
           ({
              item =
                {
                  if_type_condition = Some type_condition;
                  if_selection_set = selection;
                };
            } as field) as parent -> (
         let field_ty = Schema.lookup_type schema type_condition.item in
         match field_ty with
         | None -> parent
         | Some field_ty ->
           let selection_set =
             fn (Selection parent) schema field_ty selection.item
           in
           Graphql_ast.InlineFragment
             {
               field with
               item =
                 {
                   field.item with
                   if_selection_set = { selection with item = selection_set };
                 };
             })
       | Graphql_ast.Field
           ({ item = { fd_selection_set = Some selection } } as field) as parent
         -> (
         let field_ty = safe_get_field_type schema ty field.item.fd_name.item in
         match field_ty with
         | None -> parent
         | Some field_ty ->
           let selection_set =
             fn (Selection parent) schema field_ty selection.item
           in
           Graphql_ast.Field
             {
               field with
               item =
                 {
                   field.item with
                   fd_selection_set =
                     Some { selection with item = selection_set };
                 };
             })
       | other -> other)

let rec do_add_typename_to_selection_set parent schema ty selection_set =
  let add_typename =
    match (ty, selection_set) with
    | _, [] -> false
    | _, [ Graphql_ast.FragmentSpread _ ] -> false
    | Schema.Interface _, _selection_set -> false
    | Schema.Union _, _selection_set -> false
    | Schema.Object { om_name }, _selection_set -> (
      let open Schema in
      match schema with
      | { meta = { sm_subscription_type = Some name } } when name = om_name ->
        false
      | { meta = { sm_mutation_type = Some name } } when name = om_name -> false
      | { meta = { sm_query_type = name } } when name = om_name -> false
      | _ -> (
        match parent with
        | Selection
            (Graphql_ast.Field { item = { fd_directives = directives } }) ->
          not
            (directives
            |> List.exists
                 (fun (d : Graphql_ast.directive Source_pos.spanning) ->
                 d.item.d_name.item = "bsVariant"))
        | _ -> true))
    | _, _selection_set -> false
  in
  let selection_set =
    traverse_selection_set schema ty selection_set
      do_add_typename_to_selection_set
  in
  let already_has_typename =
    selection_set
    |> List.exists (function
         | Graphql_ast.Field { item = { fd_name = { item = "__typename" } } } ->
           true
         | _ -> false)
  in
  let parent_span = get_parent_span parent in
  if add_typename && not already_has_typename then
    Graphql_ast.Field
      {
        span = parent_span;
        item =
          {
            fd_alias = None;
            fd_name =
              {
                span =
                  ( { Source_pos.index = -1; line = -1; col = -1 },
                    { Source_pos.index = -1; line = -1; col = -1 } );
                item = "__typename";
              };
            fd_arguments = None;
            fd_directives = [];
            fd_selection_set = None;
          };
      }
    :: selection_set
  else selection_set

let rec do_remove_typename_from_union _parent schema ty selection_set =
  let selection_set =
    match ty with
    | Schema.Union _ ->
      let open Graphql_ast in
      selection_set
      |> List.fold_left
           (fun acc -> function
             | Field { item = { fd_name = { item = "__typename" } } } -> acc
             | other -> other :: acc)
           []
      |> List.rev
    | _ -> selection_set
  in
  traverse_selection_set schema ty selection_set do_remove_typename_from_union

let traverse_document_selections fn (schema : Schema.t) definitions =
  let open Graphql_ast in
  definitions
  |> List.map (fun def ->
       match def with
       | Operation { item = op; span } as parent -> (
         let ty_name =
           match op.o_type with
           | Query -> schema.meta.sm_query_type
           | Mutation ->
             Option.unsafe_unwrap ~reason:"Cannot find mutation type"
               schema.meta.sm_mutation_type
           | Subscription ->
             Option.unsafe_unwrap ~reason:"Cannot find subscription type"
               schema.meta.sm_subscription_type
         in
         let ty = Schema.lookup_type schema ty_name in
         match ty with
         | None -> parent
         | Some ty ->
           Operation
             {
               span;
               item =
                 {
                   op with
                   o_selection_set =
                     {
                       item =
                         fn (Definition parent) schema ty
                           op.o_selection_set.item;
                       span = op.o_selection_set.span;
                     };
                 };
             })
       | Fragment { item = f; span } as parent -> (
         let ty_name = f.fg_type_condition.item in
         let ty = Schema.lookup_type schema ty_name in
         match ty with
         | Some ty ->
           Fragment
             {
               item =
                 {
                   f with
                   fg_selection_set =
                     {
                       item =
                         fn (Definition parent) schema ty
                           f.fg_selection_set.item;
                       span = f.fg_selection_set.span;
                     };
                 };
               span;
             }
         | None -> parent))

let add_typename_to_selection_set =
  traverse_document_selections do_add_typename_to_selection_set

let remove_typename_from_union =
  traverse_document_selections do_remove_typename_from_union

module Visitor : Traversal_utils.VisitorSig = struct
  open Traversal_utils
  open Graphql_ast
  open Source_pos
  open Type_utils.Generic
  include AbstractVisitor

  type t = { variable_types : (string, tree) Hashtbl.t }

  let make_self () = { variable_types = Hashtbl.create 0 }

  let enter_operation_definition self ctx def =
    let () = Hashtbl.clear self.variable_types in
    match def.item.o_variable_definitions with
    | None -> ()
    | Some { item; _ } ->
        List.iter
          (fun (name, { vd_type = { item = type_ } }) ->
            Hashtbl.add self.variable_types name.item
              (type_ |> from_graphql_ast_tr ~schema:ctx.schema))
          item

  let enter_fragment_definition self ctx (def : fragment spanning) =
    let () = Hashtbl.clear self.variable_types in
    match def.item.fg_variable_definitions with
    | None -> ()
    | Some { item; _ } ->
        List.iter
          (fun (name, { vd_type = { item = type_ } }) ->
            Hashtbl.add self.variable_types name.item
              (type_ |> from_graphql_ast_tr ~schema:ctx.schema))
          item

  let rec parse_input_object self ctx (top_pos : string spanning)
      (type_ : Schema.type_ref) (value : input_value spanning) =
    match
      ( type_ |> Schema.innermost_name |> Schema.lookup_type ctx.schema,
        value.item )
    with
    | Some (InputObject obj_meta), Iv_list ({ item = Iv_object value; _ } :: _)
    | Some (InputObject obj_meta), Iv_object value ->
        obj_meta.iom_input_fields
        |> List.iter (fun { Schema.am_arg_type; am_name } ->
               let value_field =
                 value
                 |> List.find_opt (fun (name, _value) -> name.item = am_name)
               in
               (match value_field with
               | Some value_field ->
                   parse_input_object self ctx (fst value_field) am_arg_type
                     (snd value_field)
               | None -> ());
               match
                 ( am_arg_type |> from_schema_tr ~schema:ctx.schema,
                   value_field
                   |> Option.map (fun (name, value) ->
                          ( (match value.item with
                            | Iv_variable val_name ->
                                Source_pos.replace name val_name
                            | _ -> name),
                            Source_pos.replace value
                              (value.item
                              |> from_graphql_ast_iv
                                   ~arguments:self.variable_types
                                   ~schema:ctx.schema) )) )
               with
               | NonNull _, None ->
                   Traversal_utils.Context.push_error ctx top_pos.span
                     (generate_error
                        (RequiredFieldMissing (top_pos.item, am_name)))
               | T _, None | List _, None -> ()
               | key_type, Some (name, value) -> (
                   match can_apply key_type value.item with
                   | Ok -> ()
                   | Unequal ->
                       Traversal_utils.Context.push_error ctx value.span
                         (generate_error
                            (MismatchedTypes
                               ( key_type |> string_of_tree,
                                 value.item |> string_of_tree )))
                   | RequiredMismatch ->
                       Traversal_utils.Context.push_error ctx name.span
                         (generate_error
                            (MismatchedRequiredVar (am_name, name.item)))))
    | _ -> ()

  let enter_argument self ctx
      ( (name : string spanning),
        (arg_type : Schema.type_ref option),
        (value : input_value spanning) ) =
    match arg_type |> Option.map (from_schema_tr ~schema:ctx.schema) with
    | Some (T InputObject)
    | Some (NonNull (T InputObject))
    | Some (List (T InputObject))
    | Some (NonNull (List (T InputObject))) ->
        parse_input_object self ctx name
          (arg_type
          |> Option.unsafe_unwrap
               ~reason:("Can't fidn argument name: " ^ name.item))
          value
    | _ -> ()
end

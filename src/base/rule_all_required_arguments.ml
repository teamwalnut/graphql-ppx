module Visitor : Traversal_utils.VisitorSig = struct
  open Traversal_utils
  open Source_pos
  open Graphql_ast
  open Schema
  include AbstractVisitor

  let enter_field _self ctx def =
    let field_meta =
      Context.parent_type ctx
      |> Option.flat_map (fun t -> Schema.lookup_field t def.item.fd_name.item)
    in
    let provided_args =
      def.item.fd_arguments
      |> Option.map (fun span -> span.item)
      |> Option.get_or_else []
      |> List.map (fun arg -> (arg |> fst).item)
    in
    let expected_args =
      field_meta
      |> Option.map (fun fm -> fm.fm_arguments)
      |> Option.get_or_else []
      |> List.filter (fun arg ->
             match (arg.am_arg_type, arg.am_default_value) with
             | NonNull _, None -> true
             | NonNull _, Some _ | _ -> false)
    in
    expected_args
    |> List.iter (fun arg ->
           let provided =
             provided_args
             |> List.exists (fun arg_name -> arg_name = arg.am_name)
           in
           if not provided then
             let message =
               Printf.sprintf
                 ("Argument \"%s\" on field \"%s\" not provided"
                 [@reason.raw_literal
                   "Argument \\\"%s\\\" on field \\\"%s\\\" not provided"])
                 arg.am_name def.item.fd_name.item
             in
             Context.push_error ctx def.item.fd_name.span message)

  type t = unit

  let make_self () = ()
end

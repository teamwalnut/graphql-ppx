module Visitor : Traversal_utils.VisitorSig = struct
  open Traversal_utils
  open Source_pos
  open Graphql_ast
  include AbstractVisitor

  type t = unit

  let make_self () = ()

  let enter_field _ ctx def =
    let field_meta =
      Context.parent_type ctx
      |> Option.flat_map (fun t -> Schema.lookup_field t def.item.fd_name.item)
    in
    field_meta
    |> Option.map (fun (field : Schema.field_meta) ->
           match field.fm_deprecation_reason with
           | None -> ()
           | Some reason ->
               let message =
                 Printf.sprintf
                   ("Field \"%s\" has been deprecated. Reason: %s"
                   [@reason.raw_literal
                     "Field \\\"%s\\\" has been deprecated. Reason: %s"])
                   field.fm_name reason
               in
               Context.push_warning ctx def.span message)
    |> ignore
end

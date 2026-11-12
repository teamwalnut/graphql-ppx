module Visitor : Traversal_utils.VisitorSig = struct
  open Traversal_utils
  open Source_pos
  open Graphql_ast
  include AbstractVisitor

  type opts = { mutable active : bool }
  type t = opts * (string, span) Hashtbl.t

  let make_self () = ({ active = false }, Hashtbl.create 0)

  let enter_operation_definition (opts, self) _ def =
    opts.active <- true;
    let () = Hashtbl.clear self in
    match def.item.o_variable_definitions with
    | None -> ()
    | Some { item; _ } ->
      List.iter (fun (name, _) -> Hashtbl.add self name.item name.span) item

  let exit_operation_definition (opts, _) _ _ = opts.active <- false

  let enter_fragment_definition (opts, self) _ def =
    opts.active <- true;
    let () = Hashtbl.clear self in
    match def.item.fg_variable_definitions with
    | None -> ()
    | Some { item; _ } ->
      List.iter (fun (name, _) -> Hashtbl.add self name.item name.span) item

  let exit_fragment_definition (opts, _) _ _ = opts.active <- false

  let enter_variable_value (opts, self) ctx def =
    if opts.active && not (Hashtbl.mem self def.item) then
      let message =
        Printf.sprintf
          "Variable \"%s\" not found in operation. Make sure it's defined!"
          def.item
      in
      Context.push_error ctx def.span message
end

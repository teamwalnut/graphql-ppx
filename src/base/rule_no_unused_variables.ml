module Visitor : Traversal_utils.VisitorSig = struct
  open Traversal_utils
  open Source_pos
  open Graphql_ast
  include AbstractVisitor

  type t = (string, span) Hashtbl.t

  let make_self () = Hashtbl.create 0

  let enter_fragment_definition self _ def =
    let () = Hashtbl.clear self in
    match def.item.fg_variable_definitions with
    | None -> ()
    | Some { item; _ } ->
        List.iter (fun (name, _) -> Hashtbl.add self name.item name.span) item

  let exit_fragment_definition self ctx def =
    Hashtbl.iter
      (fun v span ->
        let message =
          Printf.sprintf
            ("Variable \"$%s\" is never used in fragment \"%s\""
            [@reason.raw_literal
              "Variable \\\"$%s\\\" is never used in fragment \\\"%s\\\""]) v
            def.item.fg_name.item
        in
        Context.push_error ctx span message)
      self

  let enter_operation_definition self _ def =
    let () = Hashtbl.clear self in
    match def.item.o_variable_definitions with
    | None -> ()
    | Some { item; _ } ->
        List.iter (fun (name, _) -> Hashtbl.add self name.item name.span) item

  let exit_operation_definition self ctx def =
    Hashtbl.iter
      (fun v span ->
        if v.[0] <> '_' then
          let message =
            match def.item.o_name with
            | None -> Printf.sprintf "Variable \"$%s\" is never used." v
            | Some name ->
                Printf.sprintf
                  ("Variable \"$%s\" is never used in operation \"%s\""
                  [@reason.raw_literal
                    "Variable \\\"$%s\\\" is never used in operation \\\"%s\\\""])
                  v name.item
          in
          Context.push_error ctx span message)
      self

  let enter_variable_value self _ name = Hashtbl.remove self name.item
end

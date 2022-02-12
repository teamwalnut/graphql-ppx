open Graphql_ast
open Graphql_parser
open Source_pos

let parse_argument parser =
  let open Result_ext in
  expect_name parser
  |> flat_map (fun name -> expect parser Graphql_lexer.Colon |> replace name)
  |> flat_map (fun name ->
       Graphql_parser_value.parse_value_literal false parser
       |> map (fun value -> (name, value)))

let parse_arguments parser =
  match peek parser with
  | { item = Graphql_lexer.Paren_open; _ } ->
    delimited_nonempty_list parser Graphql_lexer.Paren_open parse_argument
      Graphql_lexer.Paren_close
    |> Result_ext.map (fun args -> Some args)
  | _ -> Ok None

let parse_directive parser =
  let open Result_ext in
  expect parser Graphql_lexer.At
  |> flat_map (fun { span = at_start, _; _ } ->
       expect_name parser |> map (make_t2 at_start))
  |> flat_map (fun (at_start, name) ->
       parse_arguments parser |> map (make_t3 at_start name))
  |> map (fun (at_start, name, arguments) ->
       match arguments with
       | None ->
         start_end at_start (end_pos name) { d_name = name; d_arguments = None }
       | Some arguments ->
         start_end at_start
           (match arguments.item with
           | hd :: _ -> hd |> snd |> end_pos
           | [] -> name |> end_pos)
           { d_name = name; d_arguments = Some arguments })

let parse_directives parser =
  match peek parser with
  | { item = Graphql_lexer.At; _ } ->
    let rec scanner acc =
      match peek parser with
      | { item = Graphql_lexer.At; _ } -> (
        match parse_directive parser with
        | Error e -> Error e
        | Ok directive -> scanner (directive :: acc))
      | _ -> Ok (List.rev acc)
    in
    scanner []
  | _ -> Ok []

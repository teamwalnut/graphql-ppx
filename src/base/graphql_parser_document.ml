open Result
open Graphql_ast
open Graphql_parser
open Source_pos
open Graphql_parser_directives

let last l =
  match List.length l with 0 -> None | n -> Some (List.nth l (n - 1))

let selection_end_pos s =
  match s with
  | Field { span = _, end_pos; _ } -> end_pos
  | FragmentSpread { span = _, end_pos; _ } -> end_pos
  | InlineFragment { span = _, end_pos; _ } -> end_pos

let rec parse_type parser =
  let open Result_ext in
  skip parser Graphql_lexer.Bracket_open
  |> flat_map (function
       | Some { span = start_pos, _; _ } ->
           parse_type parser
           |> flat_map (fun inner_type ->
                  expect parser Graphql_lexer.Bracket_close
                  |> map (make_t2 inner_type))
           |> flat_map
                (fun (inner_type, { span = _, (_ as bracket_end_pos); _ }) ->
                  match peek parser with
                  | { item = Graphql_lexer.Exclamation_mark; span = _, end_pos }
                    ->
                      next parser
                      |> replace
                           (start_end start_pos end_pos
                              (Tr_non_null_list inner_type))
                  | _ ->
                      Ok
                        (start_end start_pos bracket_end_pos
                           (Tr_list inner_type)))
       | None ->
           expect_name parser
           |> flat_map (fun name ->
                  match peek parser with
                  | { item = Graphql_lexer.Exclamation_mark; span = _, end_pos }
                    ->
                      next parser
                      |> replace
                           (start_end (start_pos name) end_pos
                              (Tr_non_null_named name))
                  | _ -> Ok (Source_pos.replace name (Tr_named name))))

let parse_variable_definition parser =
  let open Result_ext in
  expect parser Graphql_lexer.Dollar
  |> flat_map (fun { span = start_pos, _; _ } ->
         expect_name parser |> map (make_t2 start_pos))
  |> flat_map (fun v -> expect parser Graphql_lexer.Colon |> replace v)
  |> flat_map (fun (start_pos, name) ->
         parse_type parser |> map (make_t3 start_pos name))
  |> flat_map (fun (start_pos, name, ty) ->
         skip parser Graphql_lexer.Equals |> map (make_t4 start_pos name ty))
  |> flat_map (fun (start_pos, name, ty, equal_sign) ->
         match equal_sign with
         | None ->
             Ok
               (start_end start_pos (end_pos ty)
                  ( start_end start_pos (end_pos name) name.item,
                    { vd_type = ty; vd_default_value = None } ))
         | Some _ ->
             Graphql_parser_value.parse_value_literal true parser
             |> map (fun default_value ->
                    start_end start_pos (end_pos default_value)
                      ( start_end start_pos (end_pos name) name.item,
                        { vd_type = ty; vd_default_value = Some default_value }
                      )))

let parse_variable_definitions parser =
  match peek parser with
  | { item = Graphql_lexer.Paren_open; _ } ->
      delimited_nonempty_list parser Graphql_lexer.Paren_open
        parse_variable_definition Graphql_lexer.Paren_close
      |> Result_ext.map (fun span ->
             Some (map (fun items -> List.map (fun s -> s.item) items) span))
  | _ -> Ok None

let rec parse_selection_set parser =
  delimited_nonempty_list parser Graphql_lexer.Curly_open parse_selection
    Graphql_lexer.Curly_close

and parse_optional_selection_set parser =
  match peek parser with
  | { item = Graphql_lexer.Curly_open; _ } ->
      Result_ext.map (fun x -> Some x) (parse_selection_set parser)
  | _ -> Ok None

and parse_field parser =
  let open Result_ext in
  let parse_rest alias name =
    parse_arguments parser
    |> flat_map (fun arguments ->
           parse_directives parser |> map (make_t2 arguments))
    |> flat_map (fun (arguments, directives) ->
           parse_optional_selection_set parser
           |> map (make_t3 arguments directives))
    |> map (fun (arguments, directives, selection_set) ->
           start_end
             (match alias with
             | Some { span = start_pos, _; _ } -> start_pos
             | None -> name |> start_pos)
             (match selection_set with
             | Some { span = _, end_pos; _ } -> end_pos
             | None -> (
                 match last directives with
                 | Some { span = _, end_pos; _ } -> end_pos
                 | None -> (
                     match arguments with
                     | Some { span = _, end_pos; _ } -> end_pos
                     | None -> name |> end_pos)))
             {
               fd_alias = alias;
               fd_name = name;
               fd_arguments = arguments;
               fd_directives = directives;
               fd_selection_set = selection_set;
             })
  in
  expect_name parser
  |> flat_map (fun alias_or_name ->
         skip parser Graphql_lexer.Colon |> map (make_t2 alias_or_name))
  |> flat_map (fun (alias_or_name, colon) ->
         match colon with
         | None -> parse_rest None alias_or_name
         | Some _ ->
             expect_name parser
             |> flat_map (fun name -> parse_rest (Some alias_or_name) name))

and parse_fragment parser =
  let open Result_ext in
  match expect parser Graphql_lexer.Ellipsis with
  | Error e -> Error e
  | Ok { span = start_pos, _; _ } -> (
      match peek parser with
      | { item = Graphql_lexer.Name "on" } ->
          next parser
          |> flat_map (fun _ -> expect_name parser)
          |> flat_map (fun name ->
                 parse_directives parser |> map (make_t2 name))
          |> flat_map (fun (name, directives) ->
                 parse_selection_set parser |> map (make_t3 name directives))
          |> map (fun (name, directives, selection_set) ->
                 InlineFragment
                   (start_end start_pos (end_pos selection_set)
                      {
                        if_type_condition = Some name;
                        if_directives = directives;
                        if_selection_set = selection_set;
                      }))
      | { item = Graphql_lexer.Curly_open; _ } ->
          parse_selection_set parser
          |> map (fun selection_set ->
                 InlineFragment
                   (start_end start_pos (end_pos selection_set)
                      {
                        if_type_condition = None;
                        if_directives = [];
                        if_selection_set = selection_set;
                      }))
      | { item = Graphql_lexer.Name _; _ } ->
          expect_dotted_name parser
          |> flat_map (fun name ->
                 parse_directives parser |> map (make_t2 name))
          |> map (fun (name, directives) ->
                 FragmentSpread
                   (start_end start_pos
                      (match last directives with
                      | Some s -> end_pos s
                      | None -> end_pos name)
                      { fs_name = name; fs_directives = directives }))
      | { item = Graphql_lexer.At; _ } ->
          parse_directives parser
          |> flat_map (fun directives ->
                 parse_selection_set parser |> map (make_t2 directives))
          |> map (fun (directives, selection_set) ->
                 InlineFragment
                   (start_end start_pos (end_pos selection_set)
                      {
                        if_type_condition = None;
                        if_directives = directives;
                        if_selection_set = selection_set;
                      }))
      | _ -> (
          match next parser with
          | Error e -> Error e
          | Ok span -> Error (Source_pos.map (fun t -> Unexpected_token t) span)
          ))

and parse_selection parser =
  match peek parser with
  | { item = Graphql_lexer.Ellipsis; _ } -> parse_fragment parser
  | _ ->
      Result_ext.map
        (fun (span : field spanning) -> Field span)
        (parse_field parser)

let parse_operation_type parser =
  match next parser with
  | Error e -> Error e
  | Ok ({ item = Graphql_lexer.Name "query"; _ } as span) ->
      Ok (replace span Query)
  | Ok ({ item = Graphql_lexer.Name "mutation"; _ } as span) ->
      Ok (replace span Mutation)
  | Ok ({ item = Graphql_lexer.Name "subscription"; _ } as span) ->
      Ok (replace span Subscription)
  | Ok span -> Error (map (fun t -> Unexpected_token t) span)

let parse_operation_definition parser =
  let open Result_ext in
  match peek parser with
  | { item = Graphql_lexer.Curly_open; _ } -> (
      match parse_selection_set parser with
      | Error e -> Error e
      | Ok span ->
          Ok
            (Source_pos.replace span
               {
                 o_type = Query;
                 o_name = None;
                 o_variable_definitions = None;
                 o_directives = [];
                 o_selection_set = span;
               }))
  | { span = start_pos, _; _ } -> (
      let parse_rest operation_type name =
        parse_variable_definitions parser
        |> flat_map (fun vdefs ->
               parse_directives parser |> map (make_t2 vdefs))
        |> flat_map (fun (vdefs, directives) ->
               parse_selection_set parser |> map (make_t3 vdefs directives))
        |> map (fun (vdefs, directives, selection_set) ->
               start_end start_pos (end_pos selection_set)
                 {
                   o_type = operation_type;
                   o_name = name;
                   o_variable_definitions = vdefs;
                   o_directives = directives;
                   o_selection_set = selection_set;
                 })
      in
      match parse_operation_type parser with
      | Error e -> Error e
      | Ok operation_type -> (
          match peek parser with
          | { item = Graphql_lexer.Name _; _ } -> (
              match expect_name parser with
              | Error e -> Error e
              | Ok name_span -> parse_rest operation_type.item (Some name_span))
          | _ -> parse_rest operation_type.item None))

let parse_string_to_type str =
  match str |> Graphql_lexer.make |> Graphql_lexer.consume with
  | Ok tokens -> (
      match parse_type (Graphql_parser.make tokens) with
      | Ok type_ -> Some type_.item
      | Error _ -> None)
  | Error _ -> None

let get_fragment_argument_definitions
    (directives : Graphql_ast.directive Source_pos.spanning list) =
  match
    directives
    |> List.find (fun d -> d.item.d_name.item = "argumentDefinitions")
  with
  | { item = { d_arguments = Some arguments; _ }; span } -> (
      let arguments =
        arguments.item
        |> List.fold_left
             (fun acc -> function
               | ( { item = key; span },
                   { item = Iv_object values; span = type_span } ) -> (
                   let type_ =
                     values
                     |> List.fold_left
                          (fun acc -> function
                            | ( { item = "type"; _ },
                                { item = Iv_string type_; _ } ) ->
                                Some type_
                            | _ -> acc)
                          None
                   in
                   match type_ with
                   | Some type_ -> (
                       match parse_string_to_type type_ with
                       | Some type_ ->
                           ( { item = key; span },
                             {
                               vd_type = { item = type_; span = type_span };
                               vd_default_value = None;
                             } )
                           :: acc
                       | None -> acc)
                   | _ -> acc)
               | _ -> acc)
             []
      in
      match arguments with
      | [] -> None
      | arguments -> Some { span; item = arguments })
  | _ -> None
  | exception Not_found -> None

let parse_fragment_definition parser =
  let open Result_ext in
  expect parser (Graphql_lexer.Name "fragment")
  |> flat_map (fun { span = start_pos, _; _ } ->
         expect_name parser |> map (make_t2 start_pos))
  |> flat_map (fun (start_pos, name) ->
         match name with
         | { item = "on"; _ } as span ->
             Error
               (Source_pos.replace span
                  (Unexpected_token (Graphql_lexer.Name "on")))
         | _ -> Ok (start_pos, name))
  |> flat_map (fun v -> expect parser (Graphql_lexer.Name "on") |> replace v)
  |> flat_map (fun (start_pos, name) ->
         expect_name parser |> map (make_t3 start_pos name))
  |> flat_map (fun (start_pos, name, type_cond) ->
         parse_directives parser |> map (make_t4 start_pos name type_cond))
  |> flat_map (fun (start_pos, name, type_cond, directives) ->
         parse_selection_set parser
         |> map (make_t5 start_pos name type_cond directives))
  |> map (fun (start_pos, name, type_cond, directives, selection_set) ->
         start_end start_pos (end_pos selection_set)
           {
             fg_name = name;
             fg_variable_definitions =
               get_fragment_argument_definitions directives;
             fg_type_condition = type_cond;
             fg_directives = directives;
             fg_selection_set = selection_set;
           })

let parse_definition parser =
  let open Result_ext in
  match peek parser with
  | { item = Graphql_lexer.Curly_open; _ }
  | { item = Graphql_lexer.Name "query"; _ }
  | { item = Graphql_lexer.Name "mutation"; _ }
  | { item = Graphql_lexer.Name "subscription"; _ } ->
      parse_operation_definition parser |> map (fun def -> Operation def)
  | { item = Graphql_lexer.Name "fragment"; _ } ->
      parse_fragment_definition parser |> map (fun def -> Fragment def)
  | span -> Error (Source_pos.map (fun t -> Unexpected_token t) span)

let parse_document parser =
  let rec scanner acc =
    match parse_definition parser with
    | Error e -> Error e
    | Ok def -> (
        match peek parser with
        | { item = Graphql_lexer.End_of_file; _ } -> Ok (List.rev (def :: acc))
        | _ -> scanner (def :: acc))
  in
  scanner []

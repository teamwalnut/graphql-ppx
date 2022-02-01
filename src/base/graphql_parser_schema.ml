open Result
open Graphql_parser
open Source_pos
open Graphql_parser_directives

let rec parse_type_ref parser =
  let open Schema in
  match next parser with
  | Ok { item = Graphql_lexer.Name name; _ } -> (
    match peek parser with
    | { item = Graphql_lexer.Exclamation_mark; _ } ->
      let _ = next parser in
      Ok (NonNull (Named name))
    | _ -> Ok (Named name))
  | Ok { item = Graphql_lexer.Bracket_open; _ } ->
    parse_type_ref parser
    |> Result_ext.flat_map (fun type_ref ->
         expect parser Graphql_lexer.Bracket_close
         |> Result_ext.map (fun _ -> type_ref))
    |> Result_ext.flat_map (fun type_ref ->
         match peek parser with
         | { item = Graphql_lexer.Exclamation_mark; span = _ } ->
           let _ = next parser in
           Ok (NonNull (List type_ref))
         | _ -> Ok (List type_ref))
  | Ok { item; span } -> Error { span; item = Unexpected_token item }
  | Error e -> Error e

let parse_default_value parser =
  match peek parser with
  | { item = Graphql_lexer.Equals } ->
    let _ = next parser in
    Graphql_parser_value.parse_value_literal true parser
    |> Result_ext.map (fun default_value ->
         Some (Graphql_printer.print_input_value default_value.item))
  | _ -> Ok None

let get_description parser =
  match peek parser with
  | { item = Graphql_lexer.String s; span = _ } ->
    let _ = next parser in
    Some s
  | _ -> None

let parse_type_argument parser =
  let open Result_ext in
  let description = get_description parser in
  expect_name parser
  |> flat_map (fun name -> replace name (expect parser Graphql_lexer.Colon))
  |> flat_map (fun name ->
       parse_type_ref parser |> map (fun type_ref -> (name, type_ref)))
  |> flat_map (fun (name, type_ref) ->
       parse_default_value parser
       |> map (fun default_value ->
            (* we should pick up the deprecated directive here, but it's not part of the schema yet *)
            let _ = parse_directives parser in
            let open Schema in
            {
              am_name = name.item;
              am_arg_type = type_ref;
              am_default_value = default_value;
              am_description = description;
            }))

let parse_type_arguments parser =
  match peek parser with
  | { item = Graphql_lexer.Paren_open; _ } ->
    delimited_nonempty_list parser Graphql_lexer.Paren_open parse_type_argument
      Graphql_lexer.Paren_close
    |> Result_ext.map (fun args -> args.item)
  | _ -> Ok []

type schema_element =
  | Type of Schema.type_meta
  | Directive of Schema.directive_meta
  | Schema of Schema.schema_meta

let parse_scalar ~description parser =
  expect_name parser
  |> Result_ext.flat_map (fun _ -> expect_name parser)
  |> Result_ext.map (fun { item = sm_name } ->
       let open Schema in
       { sm_name; sm_description = description })

let get_deprecated_directive directives =
  match directives with
  | Ok
      [
        {
          item =
            Graphql_ast.
              {
                d_name = { item = "deprecated" };
                d_arguments =
                  Some
                    {
                      item =
                        [ ({ item = "reason" }, { item = Iv_string reason }) ];
                    };
              };
        };
      ] ->
    Some reason
  | _ -> None

let parse_deprecated_directive parser =
  parse_directives parser |> get_deprecated_directive

let parse_enum_value ~description parser =
  expect_name parser
  |> Result_ext.map (fun n ->
       let evm_deprecation_reason = parse_deprecated_directive parser in
       let open Schema in
       {
         evm_name = n.item;
         evm_description = description;
         evm_deprecation_reason;
       })

let parse_enum ~description parser =
  expect_name parser
  |> Result_ext.flat_map (fun _ -> expect_name parser)
  |> Result_ext.flat_map (fun { item = em_name } ->
       expect parser Graphql_lexer.Curly_open
       |> Result_ext.map (fun _ -> em_name))
  |> Result_ext.flat_map (fun em_name ->
       let rec consume_enums acc =
         match peek parser with
         | { item = Graphql_lexer.String description } -> (
           let _ = next parser in
           match parse_enum_value ~description:(Some description) parser with
           | Ok value -> consume_enums (value :: acc)
           | Error e -> Error e)
         | { item = Graphql_lexer.Name _ } -> (
           match parse_enum_value ~description:None parser with
           | Ok value -> consume_enums (value :: acc)
           | Error e -> Error e)
         | { item = Graphql_lexer.Curly_close } ->
           let _ = next parser in
           Ok (List.rev acc)
         | { item; span } -> Error { span; item = Unexpected_token item }
         [@@tailcall]
       in
       let em_values = consume_enums [] in
       em_values
       |> Result_ext.map (fun em_values ->
            let open Schema in
            { em_name; em_description = description; em_values }))

let parse_field parser =
  let description = get_description parser in
  expect_name parser
  |> Result_ext.flat_map (fun { item = name } ->
       parse_type_arguments parser
       |> Result_ext.flat_map (fun arguments ->
            expect parser Graphql_lexer.Colon
            |> Result_ext.map (fun _ -> arguments))
       |> Result_ext.flat_map (fun arguments ->
            parse_type_ref parser
            |> Result_ext.flat_map (fun field_type ->
                 let deprecation_reason = parse_deprecated_directive parser in
                 Ok
                   (let open Schema in
                   {
                     fm_name = name;
                     fm_description = description;
                     fm_arguments = arguments;
                     fm_field_type = field_type;
                     fm_deprecation_reason = deprecation_reason;
                   }))))

let parse_fields parser =
  let rec loop acc =
    match peek parser with
    | { item = Graphql_lexer.Curly_close; _ } ->
      let _ = next parser in
      Ok (List.rev acc)
    | _ -> (
      let field = parse_field parser in
      match field with Ok field -> loop (field :: acc) | Error e -> Error e)
  in
  loop []

let parse_input_field parser = parse_type_argument parser

let rec parse_input_fields parser acc =
  match peek parser with
  | { item = Graphql_lexer.Curly_close; _ } ->
    let _ = next parser in
    Ok (List.rev acc)
  | _ -> (
    let field = parse_input_field parser in
    match field with
    | Ok field -> parse_input_fields parser (field :: acc)
    | Error e -> Error e)

let parse_implements parser =
  let rec parse_implementations parser acc =
    match expect_name parser with
    | Ok { item = name } -> (
      let acc = name :: acc in
      match peek parser with
      | { item = Graphql_lexer.Curly_open } -> Ok (List.rev acc)
      | { item = Graphql_lexer.Ampersand } -> parse_implementations parser acc
      | { item; span } ->
        print_endline "HWOWWOOWO";
        Error { span; item = Unexpected_token item })
    | Error e -> Error e
  in
  match peek parser with
  | { item = Graphql_lexer.Name "implements" } ->
    let _ = next parser in
    parse_implementations parser []
  | _ -> Ok []

let parse_object ~description parser =
  expect_name parser
  |> Result_ext.flat_map (fun _ -> expect_name parser)
  |> Result_ext.flat_map (fun { item = name; _ } ->
       parse_implements parser
       |> Result_ext.flat_map (fun implements ->
            expect parser Graphql_lexer.Curly_open
            |> Result_ext.flat_map (fun _ -> parse_fields parser)
            |> Result_ext.map (fun fields ->
                 let open Schema in
                 {
                   om_name = name;
                   om_description = description;
                   om_fields = fields;
                   om_interfaces = implements;
                 })))

let parse_interface ~description parser =
  expect_name parser
  |> Result_ext.flat_map (fun _ -> expect_name parser)
  |> Result_ext.flat_map (fun { item = name; _ } ->
       expect parser Graphql_lexer.Curly_open
       |> Result_ext.flat_map (fun _ -> parse_fields parser)
       |> Result_ext.map (fun fields ->
            let open Schema in
            { im_name = name; im_description = description; im_fields = fields }))

let rec add_to_schema parser (callback : string -> Schema.schema_meta) =
  let schema =
    expect parser Graphql_lexer.Colon
    |> Result_ext.flat_map (fun _ -> expect_name parser)
    |> Result_ext.map (fun name -> callback name.item)
  in
  match schema with
  | Ok schema -> parse_schema_fields parser schema
  | Error e -> Error e

and parse_schema_fields parser acc =
  match next parser with
  | Ok { item = Graphql_lexer.Name "query" } ->
    add_to_schema parser (fun name -> { acc with sm_query_type = name })
  | Ok { item = Graphql_lexer.Name "mutation" } ->
    add_to_schema parser (fun name -> { acc with sm_mutation_type = Some name })
  | Ok { item = Graphql_lexer.Name "subscription" } ->
    add_to_schema parser (fun name ->
      { acc with sm_subscription_type = Some name })
  | Ok { item = Graphql_lexer.Curly_close } -> Ok acc
  | Ok { item; span } -> Error { span; item = Unexpected_token item }
  | Error e -> Error e

let parse_schema parser =
  expect_name parser
  |> Result_ext.flat_map (fun _ -> expect parser Graphql_lexer.Curly_open)
  |> Result_ext.flat_map (fun _ ->
       parse_schema_fields parser
         {
           sm_query_type = "Query";
           sm_mutation_type = None;
           sm_subscription_type = None;
         })

let rec parse_union_types ?(acc = []) parser =
  let pipe = skip parser Graphql_lexer.Pipe in
  match (acc, pipe) with
  | _, Ok (Some _) | [], Ok None -> (
    match expect_name parser with
    | Ok union -> parse_union_types ~acc:(union.item :: acc) parser
    | Error e -> Error e)
  | _, _ -> Ok (List.rev acc)
  [@@tailcall]

let parse_union ~description parser =
  expect_name parser
  |> Result_ext.flat_map (fun _ -> expect_name parser)
  |> Result_ext.flat_map (fun name ->
       expect parser Graphql_lexer.Equals |> Result_ext.map (fun _ -> name.item))
  |> Result_ext.flat_map (fun name ->
       parse_union_types parser
       |> Result_ext.map (fun unions ->
            let open Schema in
            {
              um_name = name;
              um_description = description;
              um_of_types = unions;
            }))

let parse_input_object ~description parser =
  expect_name parser
  |> Result_ext.flat_map (fun _ -> expect_name parser)
  |> Result_ext.flat_map (fun { item = name; _ } ->
       expect parser Graphql_lexer.Curly_open
       |> Result_ext.flat_map (fun _ -> parse_input_fields parser [])
       |> Result_ext.map (fun fields ->
            let open Schema in
            {
              iom_name = name;
              iom_description = description;
              iom_input_fields = fields;
            }))

let get_directive_location str =
  let open Schema in
  match str with
  | "MUTATION" -> Dl_mutation
  | "SUBSCRIPTION" -> Dl_subscription
  | "FIELD" -> Dl_field
  | "FRAGMENT_DEFINITION" -> Dl_fragment_definition
  | "FRAGMENT_SPREAD" -> Dl_fragment_spread
  | "INLINE_FRAGMENT" -> Dl_inline_fragment
  | _ -> Dl_unknown

let rec parse_directive_locations ?(acc = []) parser =
  let pipe = skip parser Graphql_lexer.Pipe in
  match (acc, pipe) with
  | [], Ok None | _, Ok (Some _) -> (
    match expect_name parser with
    | Ok { item = name } ->
      parse_directive_locations ~acc:(get_directive_location name :: acc) parser
    | Error e -> Error e)
  | _ -> Ok (List.rev acc)

let parse_directive ~description:_ parser : (Schema.directive_meta, _) result =
  expect_name parser
  |> Result_ext.flat_map (fun _ -> expect parser Graphql_lexer.At)
  |> Result_ext.flat_map (fun _ -> expect_name parser)
  |> Result_ext.flat_map (fun { item = name } ->
       parse_type_arguments parser
       |> Result_ext.map (fun arguments -> (name, arguments)))
  |> Result_ext.flat_map (fun (name, arguments) ->
       expect parser (Graphql_lexer.Name "on")
       |> Result_ext.flat_map (fun _ ->
            (parse_directive_locations parser
              : (Schema.directive_location list, _) result)
            |> Result_ext.map (fun locations : Schema.directive_meta ->
                 let open Schema in
                 {
                   dm_name = name;
                   dm_locations = locations;
                   dm_arguments = arguments;
                 })))

let parse_element =
  (fun parser ->
     let description = get_description parser in
     match peek parser with
     | { item = Graphql_lexer.Name "scalar"; _ } ->
       parse_scalar ~description parser
       |> Result_ext.map (fun scalar -> Type (Scalar scalar))
     | { item = Graphql_lexer.Name "enum"; _ } ->
       parse_enum ~description parser
       |> Result_ext.map (fun enum -> Type (Enum enum))
     | { item = Graphql_lexer.Name "type"; _ } ->
       parse_object ~description parser
       |> Result_ext.map (fun obj -> Type (Object obj))
     | { item = Graphql_lexer.Name "interface"; _ } ->
       parse_interface ~description parser
       |> Result_ext.map (fun interface -> Type (Interface interface))
     | { item = Graphql_lexer.Name "union"; _ } ->
       parse_union ~description parser
       |> Result_ext.map (fun union -> Type (Union union))
     | { item = Graphql_lexer.Name "input"; _ } ->
       parse_input_object ~description parser
       |> Result_ext.map (fun input_object -> Type (InputObject input_object))
     | { item = Graphql_lexer.Name "schema"; _ } ->
       parse_schema parser |> Result_ext.map (fun schema -> Schema schema)
     | { item = Graphql_lexer.Name "directive"; _ } ->
       parse_directive ~description parser
       |> Result_ext.map (fun directive -> Directive directive)
     | { item; span } -> Error { span; item = Unexpected_token item }
    : parser -> (schema_element, parseError spanning) result)

let add_to_schema (schema : Schema.t) element =
  match element with
  | Type type_ ->
    Hashtbl.add schema.type_map (Schema.type_name type_) type_;
    schema
  | Schema meta ->
    let open Schema in
    { schema with meta }
  | Directive _ -> schema

let add_built_in_scalars type_map =
  [ "Int"; "Float"; "String"; "ID"; "Boolean" ]
  |> List.iter (fun scalar ->
       Hashtbl.add type_map scalar
         (Schema.Scalar { sm_name = scalar; sm_description = None }));
  type_map

let add_built_in_directives directive_map =
  let open Schema in
  let directives =
    [
      {
        dm_name = "include";
        dm_locations = [ Dl_field; Dl_fragment_spread; Dl_inline_fragment ];
        dm_arguments =
          [
            {
              am_name = "if";
              am_description = None;
              am_arg_type = NonNull (Named "Boolean");
              am_default_value = None;
            };
          ];
      };
      {
        dm_name = "skip";
        dm_locations = [ Dl_field; Dl_fragment_spread; Dl_inline_fragment ];
        dm_arguments =
          [
            {
              am_name = "if";
              am_description = None;
              am_arg_type = NonNull (Named "Boolean");
              am_default_value = None;
            };
          ];
      };
    ]
  in
  directives
  |> List.iter (fun directive ->
       Hashtbl.add directive_map directive.dm_name directive);
  directive_map

let parse parser =
  let schema =
    let open Schema in
    {
      meta =
        {
          sm_query_type = "Query";
          sm_mutation_type = None;
          sm_subscription_type = None;
        };
      type_map = add_built_in_scalars (Hashtbl.create 100);
      directive_map = add_built_in_directives (Hashtbl.create 10);
    }
  in
  let rec scanner schema =
    match parse_element parser with
    | Error e -> Error e
    | Ok element -> (
      match peek parser with
      | { item = Graphql_lexer.End_of_file; _ } ->
        Ok (add_to_schema schema element)
      | _ -> scanner (add_to_schema schema element))
  in
  scanner schema

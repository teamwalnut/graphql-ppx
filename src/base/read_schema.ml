exception Schema_file_not_found

let typename_field =
  {
    Schema.fm_name = "__typename";
    fm_description = None;
    fm_arguments = [];
    fm_field_type = Schema.NonNull (Schema.Named "String");
    fm_deprecation_reason = None;
  }

let map_items f t =
  let new_t = Hashtbl.create (Hashtbl.length t) in
  let mapper k v =
    let k', v' = f k v in
    Hashtbl.add new_t k' v'
  in

  let () = Hashtbl.iter mapper t in
  new_t

let map_values f = map_items (fun k v -> (k, f k v))
let some_or o v = match o with Some v -> v | None -> v

let add_typename_if_missing fields =
  let open Schema in
  if List.exists (fun { fm_name; _ } -> fm_name = "__typename") fields then
    fields
  else typename_field :: fields

exception Unknown_type_kind of string

let rec type_ref_name tr =
  let open Schema in
  match tr with
  | Named n -> n
  | NonNull i -> type_ref_name i
  | List i -> type_ref_name i

let make_enum_value_meta v =
  let open Json.Util in
  let open Schema in
  {
    evm_name = v |> member "name" |> to_string;
    evm_description = v |> member "description" |> to_string_option;
    evm_deprecation_reason =
      (if v |> member "isDeprecated" |> to_bool then
       Some
         (some_or
            (v |> member "deprecationReason" |> to_string_option)
            "(unknown)")
      else None);
  }

let rec make_type_ref v =
  let open Json.Util in
  let open Schema in
  match v |> member "kind" |> to_string with
  | "LIST" -> List (v |> member "ofType" |> make_type_ref)
  | "NON_NULL" -> NonNull (v |> member "ofType" |> make_type_ref)
  | _ -> Named (v |> member "name" |> to_string)

let make_argument_meta v =
  let open Json.Util in
  let open Schema in
  {
    am_name = v |> member "name" |> to_string;
    am_description = v |> member "description" |> to_string_option;
    am_arg_type = v |> member "type" |> make_type_ref;
    am_default_value = v |> member "defaultValue" |> to_string_option;
  }

let make_field_meta v =
  let open Json.Util in
  let open Schema in
  {
    fm_name = v |> member "name" |> to_string;
    fm_description = v |> member "description" |> to_string_option;
    fm_arguments = v |> member "args" |> to_list |> List.map make_argument_meta;
    fm_field_type = v |> member "type" |> make_type_ref;
    fm_deprecation_reason =
      (if v |> member "isDeprecated" |> to_bool then
       Some
         (some_or
            (v |> member "deprecationReason" |> to_string_option)
            "(unknown)")
      else None);
  }

let make_scalar_meta v =
  let open Json.Util in
  let open Schema in
  {
    sm_name = v |> member "name" |> to_string;
    sm_description = v |> member "description" |> to_string_option;
  }

let make_object_meta v =
  let open Json.Util in
  let open Schema in
  {
    om_name = v |> member "name" |> to_string;
    om_description = v |> member "description" |> to_string_option;
    om_fields =
      v |> member "fields" |> to_list |> List.map make_field_meta
      |> add_typename_if_missing;
    om_interfaces =
      v |> member "interfaces" |> to_list
      |> List.map (fun i -> i |> make_type_ref |> type_ref_name);
  }

let make_enum_meta v =
  let open Json.Util in
  let open Schema in
  {
    em_name = v |> member "name" |> to_string;
    em_description = v |> member "description" |> to_string_option;
    em_values =
      v |> member "enumValues" |> to_list |> List.map make_enum_value_meta;
  }

let make_interface_meta v =
  let open Json.Util in
  let open Schema in
  {
    im_name = v |> member "name" |> to_string;
    im_description = v |> member "description" |> to_string_option;
    im_fields =
      v |> member "fields" |> to_list |> List.map make_field_meta
      |> add_typename_if_missing;
  }

let make_union_meta v =
  let open Json.Util in
  let open Schema in
  {
    um_name = v |> member "name" |> to_string;
    um_description = v |> member "description" |> to_string_option;
    um_of_types =
      v |> member "possibleTypes" |> to_list
      |> List.map (fun i -> i |> make_type_ref |> type_ref_name);
  }

let make_input_object_meta v =
  let open Json.Util in
  let open Schema in
  {
    iom_name = v |> member "name" |> to_string;
    iom_description = v |> member "description" |> to_string_option;
    iom_input_fields =
      v |> member "inputFields" |> to_list |> List.map make_argument_meta;
  }

let make_type_meta _ v =
  let open Json.Util in
  match v |> member "kind" |> to_string with
  | "SCALAR" -> Schema.Scalar (make_scalar_meta v)
  | "OBJECT" -> Schema.Object (make_object_meta v)
  | "ENUM" -> Schema.Enum (make_enum_meta v)
  | "INTERFACE" -> Schema.Interface (make_interface_meta v)
  | "UNION" -> Schema.Union (make_union_meta v)
  | "INPUT_OBJECT" -> Schema.InputObject (make_input_object_meta v)
  | e -> raise @@ Unknown_type_kind e

let make_type_map type_array =
  let open Json.Util in
  let rec type_map_loop i type_map =
    if i = Array.length type_array then type_map
    else
      let () =
        Hashtbl.add type_map
          (type_array.(i) |> member "name" |> to_string)
          type_array.(i)
      in
      type_map_loop (i + 1) type_map
  in
  let type_map = type_map_loop 0 (Hashtbl.create (Array.length type_array)) in
  map_values make_type_meta type_map

let make_directive_location directive_location =
  let open Schema in
  let open Json.Util in
  match directive_location |> to_string with
  | "QUERY" -> Dl_query
  | "MUTATION" -> Dl_mutation
  | "SUBSCRIPTION" -> Dl_subscription
  | "FIELD" -> Dl_field
  | "FRAGMENT_DEFINITION" -> Dl_fragment_definition
  | "FRAGMENT_SPREAD" -> Dl_fragment_spread
  | "INLINE_FRAGMENT" -> Dl_inline_fragment
  | _ -> Dl_unknown

let make_directive_meta _ directive =
  let open Schema in
  let open Json.Util in
  {
    dm_name = directive |> member "name" |> to_string;
    dm_locations =
      directive |> member "locations" |> to_list
      |> List.map make_directive_location;
    dm_arguments =
      directive |> member "args" |> to_list |> List.map make_argument_meta;
  }

let make_directive_map directive_array =
  let open Json.Util in
  let directive_json_map = Hashtbl.create (Array.length directive_array) in
  let () =
    Array.iter
      (fun directive ->
        Hashtbl.add directive_json_map
          (directive |> member "name" |> to_string)
          directive)
      directive_array
  in
  map_values make_directive_meta directive_json_map

let make_schema_meta v =
  let open Json.Util in
  let open Schema in
  {
    sm_query_type = v |> member "queryType" |> member "name" |> to_string;
    sm_mutation_type =
      v |> member "mutationType"
      |> to_option (fun m -> m |> member "name" |> to_string);
    sm_subscription_type =
      v |> member "subscriptionType"
      |> to_option (fun s -> s |> member "name" |> to_string);
  }

let find_file_towards_root dir file =
  let rec inner dir file =
    let here_file = Filename.concat dir file in
    Log.log ("[read_schema][here_file] " ^ here_file);
    if Sys.file_exists here_file then
      let () = Log.log ("[read_schema][found] " ^ here_file) in
      Some here_file
    else if Filename.dirname dir = dir then None
    else inner (Filename.dirname dir) file
  in

  if not (Filename.is_relative file) then Some file else inner dir file

exception Ppx_cache_dir_is_not_dir

let create_dir_if_not_exist abs_path =
  if Sys.file_exists abs_path then
    let file_stat = Unix.stat abs_path in
    let open Unix in
    match file_stat.st_kind with
    | S_DIR -> ()
    | _ -> raise Ppx_cache_dir_is_not_dir
  else
    let () = Log.log ("[make_cache_dir]" ^ abs_path) in
    match Unix.mkdir abs_path 493 with
    | () -> ()
    | exception Unix.Unix_error (error, cmd, msg) -> (
      match error with
      | Unix.EEXIST -> ()
      | error ->
        Log.error_log (Unix.error_message error ^ " " ^ cmd ^ " " ^ msg);
        raise (Unix.Unix_error (error, cmd, msg)))

let ppx_cache_dir = ".graphql_ppx_cache/"
  [@@ocaml.doc
    "\n\
    \ * Naming Explaniation\n\
    \ *\n\
    \ * json_schema_rel: the path passed in from bsconfig.json \"schema\" flag\n\
    \ * json_schema: the absolute path of confirmed-exist json schema\n\
    \ * marshaled_schema: the absolute path of marshaled schema\n\
    \ "]

let get_ppx_cache_path suffix relative_to =
  let dir =
    try Sys.getenv "cur__target_dir" with _ -> Filename.dirname relative_to
  in
  let cache_dir_path = Filename.concat dir ppx_cache_dir in
  let () = create_dir_if_not_exist cache_dir_path in
  let name = Filename.basename relative_to ^ suffix in
  Filename.concat dir (ppx_cache_dir ^ name)

let get_marshaled_path = get_ppx_cache_path ".marshaled"
let get_hash_path = get_ppx_cache_path ".hash"

let parse_json_schema json_schema =
  Log.log ("[parse json schema] " ^ json_schema);
  let result = Json.Read.from_file json_schema in
  let open Json.Util in
  let open Schema in
  let schema =
    result |> member "data" |> to_option (fun json -> json |> member "__schema")
    |> function
    | Some json -> json
    | None -> result |> member "__schema"
  in
  {
    meta = make_schema_meta schema;
    type_map =
      schema |> member "types" |> to_list |> Array.of_list |> make_type_map;
    directive_map =
      schema |> member "directives" |> to_list |> Array.of_list
      |> make_directive_map;
  }

let create_marshaled_schema json_schema data =
  let marshaled_schema = get_marshaled_path json_schema in
  Log.log ("[write marshaled] " ^ marshaled_schema);
  match open_out_bin marshaled_schema with
  | exception Sys_error msg ->
    Log.error_log ("[write marshaled][Sys_error]: " ^ msg);
    raise (Sys_error msg)
  | outc ->
    Marshal.to_channel outc data [];
    close_out outc

let read_whole_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

exception LexerError of string
exception ParserError of string * int * int
exception InvalidExtension of string

let parse_graphql_schema schema_file =
  let schema_str = read_whole_file schema_file in
  match Graphql_lexer.make schema_str |> Graphql_lexer.consume with
  | Ok tokens -> (
    match Graphql_parser.make tokens |> Graphql_parser_schema.parse with
    | Ok schema -> schema
    | Error parser_error ->
      raise
        (ParserError
           ( (match parser_error.item with
             | Unexpected_token token ->
               "Unexpected token " ^ Graphql_lexer.string_of_token token
             | Unexpected_end_of_file -> "Unexpected end of file"
             | Lexer_error _ -> "Lexer error"),
             (fst parser_error.span).line,
             (snd parser_error.span).line )))
  | Error _lex_error -> raise (LexerError "lexer error!")

let parse_schema schema_file =
  match String.lowercase_ascii (Filename.extension schema_file) with
  | ".graphql" -> parse_graphql_schema schema_file
  | ".json" -> parse_json_schema schema_file
  | extension -> raise (InvalidExtension extension)

let build_schema schema_file =
  schema_file |> parse_schema |> create_marshaled_schema schema_file

let build_schema_if_dirty json_schema =
  let open Dirty_checker in
  {
    src = json_schema;
    hash_path = get_hash_path json_schema;
    dirty_callback = build_schema;
  }
  |> check

let rec read_marshaled_schema json_schema =
  let marshaled_schema = get_marshaled_path json_schema in
  Log.log ("[read marshaled] " ^ marshaled_schema);
  match open_in_bin marshaled_schema with
  | exception Sys_error msg ->
    Log.error_log ("[read marshaled][Sys_error]: " ^ msg);
    raise (Sys_error msg)
  | file ->
    let data =
      match Marshal.from_channel file with
      | data ->
        close_in file;
        data
      | exception _ ->
        close_in file;
        recovery_build json_schema
    in
    data

and recovery_build json_schema =
  let () = Log.error_log "Marshaled file is broken. Doing recovery build..." in
  build_schema_if_dirty json_schema;
  read_marshaled_schema json_schema

let get_schema maybe_schema =
  lazy
    (match
       find_file_towards_root
         (Ppx_config.root_directory ())
         (match maybe_schema with
         | Some schema_name -> schema_name
         | None -> Ppx_config.schema_file ())
     with
    | None -> raise Schema_file_not_found
    | Some json_schema ->
      build_schema_if_dirty json_schema;
      read_marshaled_schema json_schema)

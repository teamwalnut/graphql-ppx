exception Schema_file_not_found;

let typename_field = {
  Schema.fm_name: "__typename",
  fm_description: None,
  fm_arguments: [],
  fm_field_type: Schema.NonNull(Schema.Named("String")),
  fm_deprecation_reason: None,
};

let map_items = (f, t) => {
  let new_t = Hashtbl.create(Hashtbl.length(t));
  let mapper = (k, v) => {
    let (k', v') = f(k, v);
    Hashtbl.add(new_t, k', v');
  };

  let () = Hashtbl.iter(mapper, t);
  new_t;
};

let map_values = f => map_items((k, v) => (k, f(k, v)));

let some_or = (o, v) =>
  switch (o) {
  | Some(v) => v
  | None => v
  };

let add_typename_if_missing = fields =>
  Schema.(
    if (List.exists(({fm_name, _}) => fm_name == "__typename", fields)) {
      fields;
    } else {
      [typename_field, ...fields];
    }
  );

exception Unknown_type_kind(string);

let rec type_ref_name = tr =>
  Schema.(
    switch (tr) {
    | Named(n) => n
    | NonNull(i) => type_ref_name(i)
    | List(i) => type_ref_name(i)
    }
  );

let make_enum_value_meta = v =>
  Yojson.Basic.Util.(
    Schema.{
      evm_name: v |> member("name") |> to_string,
      evm_description: v |> member("description") |> to_string_option,
      evm_deprecation_reason:
        if (v |> member("isDeprecated") |> to_bool) {
          Some(
            some_or(
              v |> member("deprecationReason") |> to_string_option,
              "(unknown)",
            ),
          );
        } else {
          None;
        },
    }
  );

let rec make_type_ref = v =>
  Yojson.Basic.Util.(
    Schema.(
      switch (v |> member("kind") |> to_string) {
      | "LIST" => List(v |> member("ofType") |> make_type_ref)
      | "NON_NULL" => NonNull(v |> member("ofType") |> make_type_ref)
      | _ => Named(v |> member("name") |> to_string)
      }
    )
  );

let make_argument_meta = v =>
  Yojson.Basic.Util.(
    Schema.{
      am_name: v |> member("name") |> to_string,
      am_description: v |> member("description") |> to_string_option,
      am_arg_type: v |> member("type") |> make_type_ref,
      am_default_value: v |> member("defaultValue") |> to_string_option,
    }
  );

let make_field_meta = v =>
  Yojson.Basic.Util.(
    Schema.{
      fm_name: v |> member("name") |> to_string,
      fm_description: v |> member("description") |> to_string_option,
      fm_arguments:
        v |> member("args") |> to_list |> List.map(make_argument_meta),
      fm_field_type: v |> member("type") |> make_type_ref,
      fm_deprecation_reason:
        if (v |> member("isDeprecated") |> to_bool) {
          Some(
            some_or(
              v |> member("deprecationReason") |> to_string_option,
              "(unknown)",
            ),
          );
        } else {
          None;
        },
    }
  );

let make_scalar_meta = v =>
  Yojson.Basic.Util.(
    Schema.{
      sm_name: v |> member("name") |> to_string,
      sm_description: v |> member("description") |> to_string_option,
    }
  );

let make_object_meta = v =>
  Yojson.Basic.Util.(
    Schema.{
      om_name: v |> member("name") |> to_string,
      om_description: v |> member("description") |> to_string_option,
      om_fields:
        v
        |> member("fields")
        |> to_list
        |> List.map(make_field_meta)
        |> add_typename_if_missing,
      om_interfaces:
        v
        |> member("interfaces")
        |> to_list
        |> List.map(i => i |> make_type_ref |> type_ref_name),
    }
  );

let make_enum_meta = v =>
  Yojson.Basic.Util.(
    Schema.{
      em_name: v |> member("name") |> to_string,
      em_description: v |> member("description") |> to_string_option,
      em_values:
        v
        |> member("enumValues")
        |> to_list
        |> List.map(make_enum_value_meta),
    }
  );

let make_interface_meta = v =>
  Yojson.Basic.Util.(
    Schema.{
      im_name: v |> member("name") |> to_string,
      im_description: v |> member("description") |> to_string_option,
      im_fields:
        v
        |> member("fields")
        |> to_list
        |> List.map(make_field_meta)
        |> add_typename_if_missing,
    }
  );

let make_union_meta = v =>
  Yojson.Basic.Util.(
    Schema.{
      um_name: v |> member("name") |> to_string,
      um_description: v |> member("description") |> to_string_option,
      um_of_types:
        v
        |> member("possibleTypes")
        |> to_list
        |> List.map(i => i |> make_type_ref |> type_ref_name),
    }
  );

let make_input_object_meta = v =>
  Yojson.Basic.Util.(
    Schema.{
      iom_name: v |> member("name") |> to_string,
      iom_description: v |> member("description") |> to_string_option,
      iom_input_fields:
        v |> member("inputFields") |> to_list |> List.map(make_argument_meta),
    }
  );

let make_type_meta = (_, v) =>
  Yojson.Basic.Util.(
    switch (v |> member("kind") |> to_string) {
    | "SCALAR" => Schema.Scalar(make_scalar_meta(v))
    | "OBJECT" => Schema.Object(make_object_meta(v))
    | "ENUM" => Schema.Enum(make_enum_meta(v))
    | "INTERFACE" => Schema.Interface(make_interface_meta(v))
    | "UNION" => Schema.Union(make_union_meta(v))
    | "INPUT_OBJECT" => Schema.InputObject(make_input_object_meta(v))
    | e => raise @@ Unknown_type_kind(e)
    }
  );

let make_type_map = type_array => {
  open Yojson.Basic.Util;
  let rec type_map_loop = (i, type_map) =>
    if (i == Array.length(type_array)) {
      type_map;
    } else {
      let () =
        Hashtbl.add(
          type_map,
          type_array[i] |> member("name") |> to_string,
          type_array[i],
        );
      type_map_loop(i + 1, type_map);
    };

  let type_map = type_map_loop(0, Hashtbl.create(Array.length(type_array)));
  map_values(make_type_meta, type_map);
};

let make_directive_location = directive_location =>
  Schema.(
    Yojson.Basic.Util.(
      switch (directive_location |> to_string) {
      | "QUERY" => Dl_query
      | "MUTATION" => Dl_mutation
      | "SUBSCRIPTION" => Dl_subscription
      | "FIELD" => Dl_field
      | "FRAGMENT_DEFINITION" => Dl_fragment_definition
      | "FRAGMENT_SPREAD" => Dl_fragment_spread
      | "INLINE_FRAGMENT" => Dl_inline_fragment
      | _ => Dl_unknown
      }
    )
  );

let make_directive_meta = (_, directive) =>
  Schema.(
    Yojson.Basic.Util.{
      // dm_description: directive |> member("description") |> to_string_option,
      dm_name: directive |> member("name") |> to_string,
      dm_locations:
        directive
        |> member("locations")
        |> to_list
        |> List.map(make_directive_location),
      dm_arguments:
        directive
        |> member("args")
        |> to_list
        |> List.map(make_argument_meta),
    }
  );

let make_directive_map = directive_array => {
  open Yojson.Basic.Util;
  let directive_json_map = Hashtbl.create(Array.length(directive_array));
  let () =
    Array.iter(
      directive =>
        Hashtbl.add(
          directive_json_map,
          directive |> member("name") |> to_string,
          directive,
        ),
      directive_array,
    );
  map_values(make_directive_meta, directive_json_map);
};

let make_schema_meta = v =>
  Yojson.Basic.Util.(
    Schema.{
      sm_query_type: v |> member("queryType") |> member("name") |> to_string,
      sm_mutation_type:
        v
        |> member("mutationType")
        |> to_option(m => m |> member("name") |> to_string),
      sm_subscription_type:
        v
        |> member("subscriptionType")
        |> to_option(s => s |> member("name") |> to_string),
    }
  );

let find_file_towards_root = (dir, file) => {
  let rec inner = (dir, file) => {
    let here_file = Filename.concat(dir, file);
    Log.log("[read_schema][here_file] " ++ here_file);

    if (Sys.file_exists(here_file)) {
      let () = Log.log("[read_schema][found] " ++ here_file);
      Some(here_file);
    } else if (Filename.dirname(dir) == dir) {
      None;
    } else {
      inner(Filename.dirname(dir), file);
    };
  };

  if (!Filename.is_relative(file)) {
    Some(file);
  } else {
    inner(dir, file);
  };
};

exception Ppx_cache_dir_is_not_dir;

let create_dir_if_not_exist = abs_path =>
  if (Sys.file_exists(abs_path)) {
    let file_stat = Unix.stat(abs_path);
    Unix.(
      switch (file_stat.st_kind) {
      | S_DIR => ()
      | _ => raise(Ppx_cache_dir_is_not_dir)
      }
    );
  } else {
    let () = Log.log("[make_cache_dir]" ++ abs_path);
    switch (Unix.mkdir(abs_path, 493)) {
    | () => ()
    | exception (Unix.Unix_error(error, cmd, msg)) =>
      switch (error) {
      | Unix.EEXIST => () /* It's Ok since the build tool e.g. BuckleScript could be multi-threading */
      | error =>
        Log.error_log(Unix.error_message(error) ++ " " ++ cmd ++ " " ++ msg);
        raise(Unix.Unix_error(error, cmd, msg));
      }
    };
  };

/**
 * Naming Explaniation
 *
 * json_schema_rel: the path passed in from bsconfig.json "schema" flag
 * json_schema: the absolute path of confirmed-exist json schema
 * marshaled_schema: the absolute path of marshaled schema
 */

let ppx_cache_dir = ".graphql_ppx_cache/";

let get_ppx_cache_path = (suffix, relative_to) => {
  let dir =
    try(Sys.getenv("cur__target_dir")) {
    | _ => Filename.dirname(relative_to)
    };

  let cache_dir_path = Filename.concat(dir, ppx_cache_dir);
  let () = create_dir_if_not_exist(cache_dir_path);
  let name = Filename.basename(relative_to) ++ suffix;
  Filename.concat(dir, ppx_cache_dir ++ name);
};

let get_marshaled_path = get_ppx_cache_path(".marshaled");
let get_hash_path = get_ppx_cache_path(".hash");

let parse_json_schema = json_schema => {
  Log.log("[parse json schema] " ++ json_schema);
  let result = Yojson.Basic.from_file(json_schema);
  open Yojson.Basic.Util;
  open Schema;
  let schema =
    result
    |> member("data")
    |> to_option(json => json |> member("__schema"))
    |> (
      fun
      | Some(json) => json
      | None => result |> member("__schema")
    );
  {
    meta: make_schema_meta(schema),
    type_map:
      schema |> member("types") |> to_list |> Array.of_list |> make_type_map,
    directive_map:
      schema
      |> member("directives")
      |> to_list
      |> Array.of_list
      |> make_directive_map,
  };
};

/* marshaled schema would be placed in `.graphql_ppx_cache` dir relatively to json_schema */
let create_marshaled_schema = (json_schema, data) => {
  let marshaled_schema = get_marshaled_path(json_schema);
  Log.log("[write marshaled] " ++ marshaled_schema);
  switch (open_out_bin(marshaled_schema)) {
  | exception (Sys_error(msg)) =>
    Log.error_log("[write marshaled][Sys_error]: " ++ msg);
    raise(Sys_error(msg));
  | outc =>
    Marshal.to_channel(outc, data, []);
    close_out(outc);
  };
};

let read_whole_file = filename => {
  let ch = open_in(filename);
  let s = really_input_string(ch, in_channel_length(ch));
  close_in(ch);
  s;
};

exception LexerError(string);
exception ParserError(string, int, int);
exception InvalidExtension(string);
let parse_graphql_schema = schema_file => {
  let schema_str = read_whole_file(schema_file);
  switch (Graphql_lexer.make(schema_str) |> Graphql_lexer.consume) {
  | Ok(tokens) =>
    switch (Graphql_parser.make(tokens) |> Graphql_parser_schema.parse) {
    | Ok(schema) => schema
    | Error(parser_error) =>
      raise(
        ParserError(
          switch (parser_error.item) {
          | Unexpected_token(token) =>
            "Unexpected token " ++ Graphql_lexer.string_of_token(token)
          | Unexpected_end_of_file => "Unexpected end of file"
          | Lexer_error(_) => "Lexer error"
          },
          fst(parser_error.span).line,
          snd(parser_error.span).line,
        ),
      )
    }
  | Error(_lex_error) => raise(LexerError("lexer error!"))
  };
};

let parse_schema = schema_file => {
  switch (String.lowercase_ascii(Filename.extension(schema_file))) {
  | ".graphql" => parse_graphql_schema(schema_file)
  | ".json" => parse_json_schema(schema_file)
  | extension => raise(InvalidExtension(extension))
  };
};

/* build_schema: parse json schema and create marshaled schema */
let build_schema = schema_file =>
  schema_file |> parse_schema |> create_marshaled_schema(schema_file);

let build_schema_if_dirty = json_schema =>
  Dirty_checker.(
    {
      src: json_schema,
      hash_path: get_hash_path(json_schema),
      dirty_callback: build_schema,
    }
    |> check
  );

let rec read_marshaled_schema = json_schema => {
  let marshaled_schema = get_marshaled_path(json_schema);
  Log.log("[read marshaled] " ++ marshaled_schema);
  switch (open_in_bin(marshaled_schema)) {
  | exception (Sys_error(msg)) =>
    Log.error_log("[read marshaled][Sys_error]: " ++ msg);
    raise(Sys_error(msg));
  | file =>
    let data =
      switch (Marshal.from_channel(file)) {
      | data =>
        close_in(file);
        data;
      | exception _ =>
        close_in(file);
        recovery_build(json_schema);
      };

    data;
  };
}
and recovery_build = json_schema => {
  let () = Log.error_log("Marshaled file is broken. Doing recovery build...");
  /* we don't remove marshal file since it might result in race condition,
   * we simply let every thread noticed the broken marshal file rewrite to it */
  build_schema_if_dirty(json_schema);
  read_marshaled_schema(json_schema);
};

/* lazily read schema and check if schema file existed */
let get_schema = maybe_schema =>
  lazy(
    switch (
      find_file_towards_root(
        Ppx_config.root_directory(),
        switch (maybe_schema) {
        | Some(schema_name) => schema_name
        | None => Ppx_config.schema_file()
        },
      )
    ) {
    | None => raise(Schema_file_not_found)
    | Some(json_schema) =>
      build_schema_if_dirty(json_schema);
      read_marshaled_schema(json_schema);
    }
  );

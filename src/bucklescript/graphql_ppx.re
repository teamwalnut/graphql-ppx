open Migrate_parsetree;
open Graphql_ppx_base;
open Source_pos;

open Output_bucklescript_utils;

let add_loc = (delimLength, base, span) => {
  let (_, _, col) = Location.get_pos_info(conv_pos(base.loc_start));
  let pos_bol_start =
    base.loc_start.pos_bol
    + col
    + delimLength
    + fst(span).index
    - fst(span).col;
  let pos_bol_end =
    base.loc_start.pos_bol
    + col
    + delimLength
    + snd(span).index
    - snd(span).col;
  let start = pos_bol_start + fst(span).col;
  let end_ = pos_bol_end + snd(span).col;
  {
    loc_start: {
      pos_fname: base.loc_start.pos_fname,
      pos_lnum: base.loc_start.pos_lnum + fst(span).line,
      pos_bol: pos_bol_start,
      pos_cnum: start,
    },
    loc_end: {
      pos_fname: base.loc_start.pos_fname,
      pos_lnum: base.loc_start.pos_lnum + snd(span).line,
      pos_bol: pos_bol_end,
      pos_cnum: end_,
    },
    loc_ghost: false,
  };
};

let fmt_lex_err = err =>
  Graphql_lexer.(
    switch (err) {
    | Unknown_character(ch) => Printf.sprintf("Unknown character %c", ch)
    | Unexpected_character(ch) =>
      Printf.sprintf("Unexpected character %c", ch)
    | Unterminated_string => Printf.sprintf("Unterminated string literal")
    | Unknown_character_in_string(ch) =>
      Printf.sprintf("Unknown character in string literal: %c", ch)
    | Unknown_escape_sequence(s) =>
      Printf.sprintf("Unknown escape sequence in string literal: %s", s)
    | Unexpected_end_of_file => Printf.sprintf("Unexpected end of query")
    | Invalid_number => Printf.sprintf("Invalid number")
    }
  );

let global_records = () => Ppx_config.records();
let global_definition = () => Ppx_config.definition();
let legacy = () => Ppx_config.legacy();
let global_template_tag = () => Ppx_config.template_tag();
let global_template_tag_import = () => Ppx_config.template_tag_import();
let global_template_tag_location = () => Ppx_config.template_tag_location();

let fmt_parse_err = err =>
  Graphql_parser.(
    switch (err) {
    | Unexpected_token(t) =>
      Printf.sprintf("Unexpected token %s", Graphql_lexer.string_of_token(t))
    | Unexpected_end_of_file => "Unexpected end of query"
    | Lexer_error(err) => fmt_lex_err(err)
    }
  );

let make_error_expr = (loc, message) => {
  Ast_408.(
    Ast_mapper.extension_of_error(Location.error(~loc, message))
    |> Ast_helper.Exp.extension(~loc)
  );
};

let extract_schema_from_config = config_fields => {
  open Ast_408;
  open Asttypes;
  open Parsetree;

  let maybe_schema_field =
    try(
      Some(
        List.find(
          config_field =>
            switch (config_field) {
            | (
                {txt: Longident.Lident("schema"), _},
                {pexp_desc: Pexp_constant(Pconst_string(_, _)), _},
              ) =>
              true
            | _ => false
            },
          config_fields,
        ),
      )
    ) {
    | _ => None
    };

  switch (maybe_schema_field) {
  | Some((_, {pexp_desc: Pexp_constant(Pconst_string(schema_name, _)), _})) =>
    Some(schema_name)
  | _ => None
  };
};

let extract_template_tag_from_config = config_fields => {
  open Ast_408;
  open Asttypes;
  open Parsetree;

  let maybe_template_tag_field =
    try(
      Some(
        List.find(
          config_field =>
            switch (config_field) {
            | (
                {txt: Longident.Lident("templateTag"), _},
                {pexp_desc: Pexp_ident({txt: _}), _},
              ) =>
              true
            | _ => false
            },
          config_fields,
        ),
      )
    ) {
    | _ => None
    };

  switch (maybe_template_tag_field) {
  | Some((_, {pexp_desc: Pexp_ident({txt: lident})})) =>
    Some(
      Longident.flatten(lident)
      |> List.fold_left(
           (acc, elem) =>
             if (acc == "") {
               elem;
             } else {
               acc ++ "." ++ elem;
             },
           "",
         ),
    )
  | _ => None
  };
};

let extract_bool_from_config = (name, config_fields) => {
  open Ast_408;
  open Asttypes;
  open Parsetree;

  let maybe_field_value =
    try(
      Some(
        List.find(
          config_field =>
            switch (config_field) {
            | (
                {txt: Longident.Lident(matched_name), _},
                {
                  pexp_desc:
                    Pexp_construct({txt: Longident.Lident(_value)}, _),
                  _,
                },
              )
                when matched_name == name =>
              true
            | _ => false
            },
          config_fields,
        ),
      )
    ) {
    | _ => None
    };

  switch (maybe_field_value) {
  | Some((
      _,
      {pexp_desc: Pexp_construct({txt: Longident.Lident(value)}, _), _},
    )) =>
    switch (value) {
    | "true" => Some(true)
    | "false" => Some(false)
    | _ => None
    }
  | _ => None
  };
};

let extract_string_from_config = (name, config_fields) => {
  open Ast_408;
  open Asttypes;
  open Parsetree;

  let maybe_string_field =
    try(
      Some(
        List.find(
          config_field =>
            switch (config_field) {
            | (
                {txt: Longident.Lident(matched_name), _},
                {pexp_desc: Pexp_constant(Pconst_string(_, _)), _},
              )
                when matched_name == name =>
              true
            | _ => false
            },
          config_fields,
        ),
      )
    ) {
    | _ => None
    };

  switch (maybe_string_field) {
  | Some((_, {pexp_desc: Pexp_constant(Pconst_string(value, _)), _})) =>
    Some(value)
  | _ => None
  };
};

let extract_template_tag_location_from_config =
  extract_string_from_config("templateTagLocation");
let extract_template_tag_import_from_config =
  extract_string_from_config("templateTagImport");

let extract_records_from_config = extract_bool_from_config("records");
let extract_objects_from_config = extract_bool_from_config("objects");
let extract_inline_from_config = extract_bool_from_config("inline");
let extract_definition_from_config = extract_bool_from_config("definition");
let extract_tagged_template_config =
  extract_bool_from_config("taggedTemplate");

type query_config = {
  schema: option(string),
  records: option(bool),
  objects: option(bool),
  inline: option(bool),
  definition: option(bool),
  template_tag: option(string),
  template_tag_location: option(string),
  template_tag_import: option(string),
  tagged_template: option(bool),
};

let get_query_config = fields => {
  {
    schema: extract_schema_from_config(fields),
    records: extract_records_from_config(fields),
    objects: extract_objects_from_config(fields),
    inline: extract_inline_from_config(fields),
    definition: extract_definition_from_config(fields),
    template_tag: extract_template_tag_from_config(fields),
    template_tag_import: extract_template_tag_import_from_config(fields),
    template_tag_location: extract_template_tag_location_from_config(fields),
    tagged_template: extract_tagged_template_config(fields),
  };
};
let empty_query_config = {
  schema: None,
  records: None,
  objects: None,
  inline: None,
  definition: None,
  tagged_template: None,
  template_tag: None,
  template_tag_location: None,
  template_tag_import: None,
};

let get_with_default = (value, default_value) => {
  switch (value) {
  | Some(value) => Some(value)
  | None => default_value
  };
};

let get_template_tag = query_config => {
  switch (query_config.tagged_template) {
  | Some(false) => (None, None, None)
  | _ =>
    switch (
      get_with_default(query_config.template_tag, global_template_tag()),
      get_with_default(
        query_config.template_tag_location,
        global_template_tag_location(),
      ),
      get_with_default(
        query_config.template_tag_import,
        global_template_tag_import(),
      ),
    ) {
    | (Some(tag), Some(location), Some(import)) => (
        Some(tag),
        Some(location),
        Some(import),
      )
    | (None, Some(location), Some(import)) => (
        Some(import),
        Some(location),
        Some(import),
      )
    | (Some(tag), Some(location), None) => (
        Some(tag),
        Some(location),
        Some("default"),
      )
    | (Some(tag), None, Some(_))
    | (Some(tag), None, None) => (Some(tag), None, None)
    | (None, _, _) => (None, None, None)
    }
  };
};

let rewrite_query =
    (~query_config: query_config, ~loc, ~delim, ~query, ~module_name, ()) => {
  open Ast_408;
  open Ast_helper;

  let lexer = Graphql_lexer.make(query);
  let delimLength =
    switch (delim) {
    | Some(s) => 2 + String.length(s)
    | None => 1
    };
  switch (Graphql_lexer.consume(lexer)) {
  | Result.Error(e) =>
    raise(
      Location.Error(
        Location.error(
          ~loc=add_loc(delimLength, loc, e.span) |> conv_loc,
          fmt_lex_err(e.item),
        ),
      ),
    )
  | Result.Ok(tokens) =>
    let parser = Graphql_parser.make(tokens);
    switch (Graphql_parser_document.parse_document(parser)) {
    | Result.Error(e) =>
      raise(
        Location.Error(
          Location.error(
            ~loc=add_loc(delimLength, loc, e.span) |> conv_loc,
            fmt_parse_err(e.item),
          ),
        ),
      )
    | Result.Ok(document) =>
      let schema = Lazy.force(Read_schema.get_schema(query_config.schema));
      let document =
        (
          if (Ppx_config.apollo_mode()) {
            document |> Ast_transforms.add_typename_to_selection_set(schema);
          } else {
            document;
          }
        )
        |> Ast_transforms.remove_typename_from_union(schema);
      let template_tag = get_template_tag(query_config);

      let config = {
        Generator_utils.map_loc: add_loc(delimLength, loc),
        delimiter: delim,
        full_document: document,
        records:
          switch (query_config.records, query_config.objects) {
          | (Some(value), _) => value
          | (_, Some(true)) => false
          | (_, Some(false)) => true
          | (None, None) => global_records()
          },
        inline:
          switch (query_config.inline) {
          | Some(value) => value
          | None => false
          },
        definition:
          switch (query_config.definition) {
          | Some(value) => value
          | None => global_definition()
          },
        legacy: legacy(),
        /*  the only call site of schema, make it lazy! */
        schema,
        template_tag,
      };
      switch (Validations.run_validators(config, document)) {
      | (Some(errs), _) =>
        let errs =
          errs
          |> List.rev
          |> List.map(((loc, msg)) => {
               let loc = conv_loc(loc);
               %stri
               [%e make_error_expr(loc, msg)];
             });
        [errs];
      | (None, warnings) =>
        warnings
        |> List.iter(((loc, message)) => {
             let loc = conv_loc(loc);
             let loc_as_ghost = {...loc, loc_ghost: true};
             Location.print_alert(
               loc,
               Location.formatter_for_warnings^,
               {
                 kind: "deprecated",
                 message,
                 def: loc_as_ghost,
                 use: loc_as_ghost,
               },
             );
             ();
           });
        Result_decoder.unify_document_schema(config, document)
        |> Output_bucklescript_module.generate_modules(config, module_name);
      };
    };
  };
};

// Default configuration
let () = Bucklescript_config.read_config();

let mapper = (_config, _cookies) => {
  Ast_408.(
    Ast_mapper.(
      Parsetree.(
        Asttypes.{
          ...default_mapper,
          structure: (mapper, struc) => {
            struc
            |> List.fold_left(
                 acc =>
                   fun
                   | {
                       pstr_desc:
                         Pstr_module({
                           pmb_name: {txt: _},
                           pmb_expr: {
                             pmod_desc:
                               Pmod_extension(({txt: "graphql", loc}, pstr)),
                           },
                         }),
                     } as item
                   | {
                       pstr_desc:
                         Pstr_eval(
                           {
                             pexp_desc:
                               Pexp_extension(({txt: "graphql", loc}, pstr)),
                           },
                           _,
                         ),
                     } as item
                   | {
                       pstr_desc:
                         Pstr_value(
                           _,
                           [
                             {
                               pvb_pat: {ppat_desc: _},
                               pvb_expr: {
                                 pexp_desc:
                                   Pexp_extension((
                                     {txt: "graphql", loc},
                                     pstr,
                                   )),
                               },
                             },
                           ],
                         ),
                     } as item => {
                       let module_name =
                         switch (item) {
                         | {pstr_desc: Pstr_module({pmb_name: {txt: name}})} =>
                           Some(name)
                         | _ => None
                         };
                       Output_bucklescript_docstrings.for_module_information(
                         loc,
                         module_name,
                       );
                       switch (pstr) {
                       | PStr([
                           {
                             pstr_desc:
                               Pstr_eval(
                                 {
                                   pexp_loc: loc,
                                   pexp_desc:
                                     Pexp_constant(
                                       Pconst_string(query, delim),
                                     ),
                                   _,
                                 },
                                 _,
                               ),
                             _,
                           },
                           {
                             pstr_desc:
                               Pstr_eval(
                                 {pexp_desc: Pexp_record(fields, None), _},
                                 _,
                               ),
                             _,
                           },
                         ]) =>
                         List.append(
                           rewrite_query(
                             ~query_config=get_query_config(fields),
                             ~loc=conv_loc_from_ast(loc),
                             ~delim,
                             ~query,
                             ~module_name,
                             (),
                           )
                           |> List.concat
                           |> List.rev,
                           acc,
                         )
                       | PStr([
                           {
                             pstr_desc:
                               Pstr_eval(
                                 {
                                   pexp_loc: loc,
                                   pexp_desc:
                                     Pexp_constant(
                                       Pconst_string(query, delim),
                                     ),
                                   _,
                                 },
                                 _,
                               ),
                             _,
                           },
                         ]) =>
                         List.append(
                           rewrite_query(
                             ~query_config=empty_query_config,
                             ~loc=conv_loc_from_ast(loc),
                             ~delim,
                             ~query,
                             ~module_name,
                             (),
                           )
                           |> List.concat
                           |> List.rev,
                           acc,
                         )
                       | _ =>
                         raise(
                           Location.Error(
                             Location.error(
                               ~loc,
                               "[%graphql] accepts a string, e.g. [%graphql {| { query |}]",
                             ),
                           ),
                         )
                       };
                     }
                   | other => [
                       default_mapper.structure_item(mapper, other),
                       ...acc,
                     ],
                 [],
               )
            |> List.rev;
          },
        }
      )
    )
  );
};

let argKey = ref("");
let args = [
  (
    "-custom-field",
    Arg.Tuple([
      Arg.String(key => {argKey := key}),
      Arg.String(
        moduleKey => {
          Hashtbl.add(Ppx_config.custom_fields(), argKey^, moduleKey)
        },
      ),
    ]),
    "Adds a global custom field decoder/serializer (format: -custom-field ScalarType Module)",
  ),
  (
    "-verbose",
    Arg.Unit(
      () =>
        Ppx_config.update_config(current =>
          {...current, verbose_logging: true}
        ),
    ),
    "Defines if logging should be verbose or not",
  ),
  (
    "-apollo-mode",
    Arg.Unit(
      () =>
        Ppx_config.update_config(current => {...current, apollo_mode: true}),
    ),
    "Automatically add __typename everywhere (necessary for apollo)",
  ),
  (
    "-schema",
    Arg.String(
      schema_file =>
        Ppx_config.update_config(current => {...current, schema_file}),
    ),
    "<path/to/schema.json>",
  ),
  (
    "-ast-out",
    Arg.Bool(
      ast_out =>
        Ppx_config.update_config(current =>
          {
            ...current,
            output_mode: ast_out ? Ppx_config.Apollo_AST : Ppx_config.String,
          }
        ),
    ),
    "Defines if output string or AST",
  ),
  (
    "-o",
    Arg.Unit(
      () =>
        Ppx_config.update_config(current =>
          {...current, verbose_error_handling: true}
        ),
    ),
    "Verbose error handling. If not defined NODE_ENV will be used",
  ),
  (
    "-objects",
    Arg.Unit(
      () => Ppx_config.update_config(current => {...current, records: false}),
    ),
    "Compile to objects instead of records by default (legacy)",
  ),
  (
    "-records",
    Arg.Unit(
      () => Ppx_config.update_config(current => {...current, records: true}),
    ),
    "Compile to records by default",
  ),
  (
    "-legacy",
    Arg.Unit(
      () =>
        Ppx_config.update_config(current =>
          {...current, legacy: true, records: false}
        ),
    ),
    "Legacy mode (make, makeWithVariables, and objects by default)",
  ),
  (
    "-no-definition",
    Arg.Unit(
      () =>
        Ppx_config.update_config(current => {...current, definition: false}),
    ),
    "Legacy mode (make and makeWithVariables)",
  ),
  (
    "-template-tag",
    Arg.String(
      template_tag =>
        Ppx_config.update_config(current =>
          {...current, template_tag: Some(template_tag)}
        ),
    ),
    "Template tag to use",
  ),
  (
    "-template-tag-import",
    Arg.String(
      template_tag_import =>
        Ppx_config.update_config(current =>
          {...current, template_tag_import: Some(template_tag_import)}
        ),
    ),
    "the import to use for the template tag (default is \"default\"",
  ),
  (
    "-template-tag-location",
    Arg.String(
      template_tag_location =>
        Ppx_config.update_config(current =>
          {...current, template_tag_location: Some(template_tag_location)}
        ),
    ),
    "the import location for the template tag (default is \"default\"",
  ),
];

let () =
  Migrate_parsetree.(
    Driver.register(~name="graphql", ~args, Versions.ocaml_408, mapper)
  );

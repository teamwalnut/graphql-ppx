open Migrate_parsetree;
open Graphql_ppx_base;
open Source_pos;

open Output_native_utils;

let argv = Sys.argv |> Array.to_list;

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

let fmt_parse_err = err =>
  Graphql_parser.(
    switch (err) {
    | Unexpected_token(t) =>
      Printf.sprintf("Unexpected token %s", Graphql_lexer.string_of_token(t))
    | Unexpected_end_of_file => "Unexpected end of query"
    | Lexer_error(err) => fmt_lex_err(err)
    }
  );

let is_prefixed = (prefix, str) => {
  let i = 0;
  let len = String.length(prefix);
  let j = ref(0);
  while (j^ < len
         && String.unsafe_get(prefix, j^) == String.unsafe_get(str, i + j^)) {
    incr(j);
  };
  j^ == len;
};

let make_error_expr = (loc, message) => {
  open Ast_408;
  let ext = Ast_mapper.extension_of_error(Location.error(~loc, message));
  Ast_helper.Exp.extension(~loc, ext);
};

let drop_prefix = (prefix, str) => {
  let len = String.length(prefix);
  let rest = String.length(str) - len;
  String.sub(str, len, rest);
};

let rewrite_query = (~schema=?, ~loc, ~delim, ~query, ()) => {
  open Ast_408;

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
      let config = {
        Generator_utils.map_loc: add_loc(delimLength, loc),
        delimiter: delim,
        full_document: document,
        /*  the only call site of schema, make it lazy! */
        schema: Lazy.force(Read_schema.get_schema(schema)),
        template_tag: (None, None, None),
        records: false,
        inline: false,
        legacy: false,
        future_added_value: Ppx_config.future_added_value(),
        extend: None,
      };
      switch (Validations.run_validators(config, document)) {
      | (Some(errs), _) =>
        Ast_helper.Mod.mk(
          Pmod_structure(
            List.map(
              ((loc, msg)) => {
                let loc = conv_loc(loc);
                %stri
                [%e make_error_expr(loc, msg)];
              },
              errs,
            ),
          ),
        )
      | (None, warnings) =>
        warnings
        |> List.iter(((loc, message)) => {
             let loc = conv_loc(loc);
             Location.print_warning(
               loc,
               Location.formatter_for_warnings^,
               Warnings.Preprocessor(message),
             );
           });
        let parts = Result_decoder.unify_document_schema(config, document);
        Output_native_module.generate_modules(config, parts);
      };
    };
  };
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

// Default configuration
let () =
  Ppx_config.(
    set_config({
      verbose_logging: false,
      output_mode: Ppx_config.String,
      verbose_error_handling:
        switch (Sys.getenv("NODE_ENV")) {
        | "production" => false
        | _ => true
        | exception Not_found => true
        },
      apollo_mode: false,
      schema_file: "graphql_schema.json",
      root_directory: Sys.getcwd(),
      raise_error_with_loc: (loc, message) => {
        let loc = conv_loc(loc);
        raise(Location.Error(Location.error(~loc, message)));
      },
      records: false,
      legacy: true,
      template_tag: None,
      template_tag_location: None,
      template_tag_import: None,
      custom_fields: Hashtbl.create(0),
      future_added_value: true,
      extend_query: None,
      extend_query_no_required_variables: None,
      extend_mutation: None,
      extend_mutation_no_required_variables: None,
      extend_subscription: None,
      extend_subscription_no_required_variables: None,
      extend_fragment: None,
    })
  );

let mapper = (_config, _cookies) => {
  Ast_408.(
    Ast_mapper.(
      Parsetree.(
        Asttypes.{
          ...default_mapper,
          module_expr: (mapper, mexpr) =>
            switch (mexpr) {
            | {pmod_desc: Pmod_extension(({txt: "graphql", loc}, pstr)), _} =>
              switch (pstr) {
              | PStr([
                  {
                    pstr_desc:
                      Pstr_eval(
                        {
                          pexp_loc: loc,
                          pexp_desc:
                            Pexp_constant(Pconst_string(query, delim)),
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
                let maybe_schema = extract_schema_from_config(fields);
                rewrite_query(
                  ~schema=?maybe_schema,
                  ~loc=conv_loc_from_ast(loc),
                  ~delim,
                  ~query,
                  (),
                );
              | PStr([
                  {
                    pstr_desc:
                      Pstr_eval(
                        {
                          pexp_loc: loc,
                          pexp_desc:
                            Pexp_constant(Pconst_string(query, delim)),
                          _,
                        },
                        _,
                      ),
                    _,
                  },
                ]) =>
                rewrite_query(
                  ~loc=conv_loc_from_ast(loc),
                  ~delim,
                  ~query,
                  (),
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
              }
            | other => default_mapper.module_expr(mapper, other)
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
    "Defines if apply Apollo specific code generation",
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
    "-future-added-value",
    Arg.Bool(
      future_added_value =>
        Ppx_config.update_config(current => {...current, future_added_value}),
    ),
    "Omits the `FutureAddedValue variant for enums if set to false",
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
];

let () =
  Migrate_parsetree.(
    Driver.register(~name="graphql", ~args, Versions.ocaml_408, mapper)
  );

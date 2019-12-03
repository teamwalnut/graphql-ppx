open Graphql_ppx_base;
open Source_pos;

open Output_bucklescript_utils;

let add_pos = (delimLength, base, pos) => {
  let (_, _, col) = Location.get_pos_info(conv_pos(base));
  {
    pos_fname: base.pos_fname,
    pos_lnum: base.pos_lnum + pos.line,
    pos_bol: 0,
    pos_cnum:
      switch (pos.line) {
      | 0 => delimLength + col + pos.col
      | _ => pos.col
      },
  };
};

let add_loc = (delimLength, base, span) => {
  loc_start: add_pos(delimLength, base.loc_start, fst(span)),
  loc_end: add_pos(delimLength, base.loc_start, snd(span)),
  loc_ghost: false,
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

let make_error_expr = (loc, message) => {
  Ast_406.(
    Ast_mapper.extension_of_error(Location.error(~loc, message))
    |> Ast_helper.Exp.extension(~loc)
  );
};

let rewrite_query = (loc, delim, query, maybe_schema) => {
  open Ast_406;
  open Ast_helper;
  open Parsetree;
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
        schema: Lazy.force(Read_schema.get_schema(maybe_schema)),
      };
      switch (Validations.run_validators(config, document)) {
      | Some(errs) =>
        Mod.mk(
          Pmod_structure(
            errs
            |> List.map(((loc, msg)) => {
                 let loc = conv_loc(loc);
                 %stri
                 [%e make_error_expr(loc, msg)];
               }),
          ),
        )
      | None =>
        Result_decoder.unify_document_schema(config, document)
        |> Output_bucklescript_module.generate_modules(config)
      };
    };
  };
};

let extract_schema_from_config = config_fields => {
  open Ast_406;
  open Asttypes;
  open Parsetree;

  let maybe_schema_field =
    try (
      Some(
        List.find(
          config_field =>
            switch (config_field) {
            | (
                {txt: Longident.Lident("schema"), _},
                {
                  pexp_desc: Pexp_constant(Pconst_string(schema_name, _)),
                  _,
                },
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

let mapper = (_config, _cookies) => {
  open Ast_406;
  open Ast_mapper;
  open Parsetree;
  open Asttypes;

  let () =
    Ppx_config.(
      set_config({
        verbose_logging: false,
        // switch (List.find((==)("-verbose"), argv)) {
        // | _ => true
        // | exception Not_found => false
        // },
        output_mode: Ppx_config.String,
        // switch (List.find((==)("-ast-out"), argv)) {
        // | _ => Ppx_config.Apollo_AST
        // | exception Not_found =>
        // },
        verbose_error_handling:
          switch (Sys.getenv("NODE_ENV")) {
          | "production" => false
          | _ => true
          | exception Not_found => true
          },
        apollo_mode:
          switch (Sys.getenv("GRAPHQL_PPX_APOLLO_MODE")) {
          | "true" => true
          | _ => false
          | exception Not_found => false
          },
        root_directory: Sys.getcwd(),
        schema_file:
          switch (Sys.getenv("GRAPHQL_PPX_SCHEMA")) {
          | arg => arg
          | exception Not_found => "graphql_schema.json"
          },
        raise_error_with_loc: (loc, message) => {
          let loc = conv_loc(loc);
          raise(Location.Error(Location.error(~loc, message)));
        },
      })
    );

  {
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
                    pexp_desc: Pexp_constant(Pconst_string(query, delim)),
                    _,
                  },
                  _,
                ),
              _,
            },
            {
              pstr_desc:
                Pstr_eval({pexp_desc: Pexp_record(fields, None), _}, _),
              _,
            },
          ]) =>
          let maybe_schema = extract_schema_from_config(fields);
          rewrite_query(conv_loc_from_ast(loc), delim, query, maybe_schema);
        | PStr([
            {
              pstr_desc:
                Pstr_eval(
                  {
                    pexp_loc: loc,
                    pexp_desc: Pexp_constant(Pconst_string(query, delim)),
                    _,
                  },
                  _,
                ),
              _,
            },
          ]) =>
          rewrite_query(conv_loc_from_ast(loc), delim, query, None)
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
  };
};

let () =
  Migrate_parsetree.Driver.register(
    ~name="graphql",
    ~args=[],
    Migrate_parsetree.Versions.ocaml_406,
    mapper,
  );

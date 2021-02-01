open Graphql_ppx_base;
open Source_pos;
open Output_bucklescript_utils;
open Ppxlib;

// not available in all supported versions of OCaml so inlining here
let filter_map = f => {
  let rec aux = accu =>
    fun
    | [] => List.rev(accu)
    | [x, ...l] =>
      switch (f(x)) {
      | None => aux(accu, l)
      | Some(v) => aux([v, ...accu], l)
      };

  aux([]);
};

let add_loc = (delimLength, base, span) => {
  let (_, _, col) = Ocaml_common.Location.get_pos_info(base.loc_start);
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

let make_error_expr = (loc, message) => {
  let error = Ocaml_common.Location.error(~loc, message);
  let ext = Ocaml_common.Ast_mapper.extension_of_error(error);
  let extension = Ocaml_common.Ast_helper.Exp.extension(~loc, ext);

  To_ppxlib.copy_expression(extension);
};

let extract_schema_from_config = config_fields => {
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
                {pexp_desc: Pexp_constant(Pconst_string(_, _, _)), _},
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
  | Some((
      _,
      {pexp_desc: Pexp_constant(Pconst_string(schema_name, _, _)), _},
    )) =>
    Some(schema_name)
  | _ => None
  };
};

let extract_template_tag_from_config = config_fields => {
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
                {pexp_desc: Pexp_constant(Pconst_string(_, _, _)), _},
              )
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
  | Some((
      _,
      {pexp_desc: Pexp_constant(Pconst_string(template_tag, _, _))},
    )) =>
    Some(template_tag)
  | Some((_, {pexp_desc: Pexp_ident({txt: lident})})) =>
    Some(
      Ocaml_common.Longident.flatten(lident)
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
                {pexp_desc: Pexp_constant(Pconst_string(_, _, _)), _},
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
  | Some((_, {pexp_desc: Pexp_constant(Pconst_string(value, _, _)), _})) =>
    Some(value)
  | _ => None
  };
};

let extract_template_tag_location_from_config =
  extract_string_from_config("templateTagLocation");
let extract_template_tag_import_from_config =
  extract_string_from_config("templateTagImport");
let extract_template_tag_return_type_from_config =
  extract_string_from_config("templateTagReturnType");
let extract_extend_from_config = extract_string_from_config("extend");

let extract_fragment_in_query_from_config = config_fields => {
  Ppx_config.(
    switch (extract_string_from_config("fragmentInQuery", config_fields)) {
    | Some("include") => Some(Include)
    | Some("exclude") => Some(Exclude)
    | _ => None
    }
  );
};

let extract_records_from_config = extract_bool_from_config("records");
let extract_objects_from_config = extract_bool_from_config("objects");
let extract_inline_from_config = extract_bool_from_config("inline");
let extract_apollo_mode_from_config = extract_bool_from_config("apolloMode");
let extract_future_added_value_from_config =
  extract_bool_from_config("futureAddedValue");
let extract_tagged_template_config =
  extract_bool_from_config("taggedTemplate");

type query_config = Result_decoder.query_config;

let get_query_config_from_trailing_record = fields =>
  Result_decoder.(
    {
      {
        schema: extract_schema_from_config(fields),
        records: extract_records_from_config(fields),
        objects: extract_objects_from_config(fields),
        inline: extract_inline_from_config(fields),
        template_tag: extract_template_tag_from_config(fields),
        template_tag_import: extract_template_tag_import_from_config(fields),
        template_tag_location:
          extract_template_tag_location_from_config(fields),
        template_tag_return_type:
          extract_template_tag_return_type_from_config(fields),
        tagged_template: extract_tagged_template_config(fields),
        future_added_value: extract_future_added_value_from_config(fields),
        extend: extract_extend_from_config(fields),
        fragment_in_query: extract_fragment_in_query_from_config(fields),
        apollo_mode: extract_apollo_mode_from_config(fields),
        // do not support these new options in the trailing record
        template_tag_is_function: None,
      };
    }
  );

let empty_query_config =
  Result_decoder.{
    schema: None,
    records: None,
    objects: None,
    inline: None,
    tagged_template: None,
    template_tag: None,
    template_tag_location: None,
    template_tag_import: None,
    template_tag_return_type: None,
    template_tag_is_function: None,
    future_added_value: None,
    extend: None,
    fragment_in_query: None,
    apollo_mode: None,
  };

let get_with_default = (value, default_value) => {
  switch (value) {
  | Some(value) => Some(value)
  | None => default_value
  };
};

let run_validations = (config, definition) => {
  switch (Validations.run_validators(config, [definition])) {
  | (Some(errs), _) =>
    Some(
      errs
      |> List.rev
      |> List.map(((loc, msg)) => {
           let loc = conv_loc(loc);
           %stri
           [%e make_error_expr(loc, msg)];
         }),
    )
  | (None, warnings) =>
    warnings
    |> List.iter(((loc, message)) => {
         let loc = conv_loc(loc);
         Ocaml_common.Location.print_warning(
           loc,
           Ocaml_common.Location.formatter_for_warnings^,
           Ocaml_common.Warnings.Preprocessor(message),
         );
       });
    None;
  };
};

let rewrite_definition_interface =
    (~query_config: query_config, ~loc, ~delim, ~query, ~module_name, ()) => {
  let lexer = Graphql_lexer.make(query);
  let delimLength =
    switch (delim) {
    | Some(s) => 2 + String.length(s)
    | None => 1
    };

  switch (Graphql_lexer.consume(lexer)) {
  | Result.Error(e) =>
    Location.raise_errorf(
      ~loc=add_loc(delimLength, loc, e.span),
      "%s",
      fmt_lex_err(e.item),
    )

  | Result.Ok(tokens) =>
    let parser = Graphql_parser.make(tokens);
    switch (Graphql_parser_document.parse_document(parser)) {
    | Result.Error(e) =>
      Location.raise_errorf(
        ~loc=add_loc(delimLength, loc, e.span),
        "%s",
        fmt_parse_err(e.item),
      )

    | Result.Ok(document) =>
      let document_with_config =
        Result_decoder.generate_config(
          ~map_loc=add_loc(delimLength, loc),
          ~delimiter=delim,
          ~initial_query_config=query_config,
          document,
        );

      document_with_config
      |> Result_decoder.unify_document_schema
      |> Output_bucklescript_module.generate_module_interfaces(module_name);
    };
  };
};

let rewrite_definition =
    (
      ~query_config: query_config,
      ~loc,
      ~delim,
      ~query,
      ~module_name,
      ~module_type,
      (),
    ) => {
  let lexer = Graphql_lexer.make(query);
  let delimLength =
    switch (delim) {
    | Some(s) => 2 + String.length(s)
    | None => 1
    };

  switch (Graphql_lexer.consume(lexer)) {
  | Result.Error(e) =>
    Location.raise_errorf(
      ~loc=add_loc(delimLength, loc, e.span),
      "%s",
      fmt_lex_err(e.item),
    )
  | Result.Ok(tokens) =>
    let parser = Graphql_parser.make(tokens);
    switch (Graphql_parser_document.parse_document(parser)) {
    | Result.Error(e) =>
      Location.raise_errorf(
        ~loc=add_loc(delimLength, loc, e.span),
        "%s",
        fmt_parse_err(e.item),
      )
    | Result.Ok(document) =>
      let document_with_config =
        Result_decoder.generate_config(
          ~map_loc=add_loc(delimLength, loc),
          ~delimiter=delim,
          ~initial_query_config=query_config,
          document,
        );

      let errors =
        document_with_config
        |> List.fold_left(
             (acc, (definition, config)) => {
               switch (run_validations(config, definition)) {
               | Some(error) => [error, ...acc]
               | None => acc
               }
             },
             [],
           )
        |> List.rev;

      switch (errors) {
      | [] =>
        try(
          document_with_config
          |> Result_decoder.unify_document_schema
          |> Output_bucklescript_module.generate_modules(
               module_name,
               module_type,
             )
        ) {
        | Output_bucklescript_module.Cant_find_fragment_type_with_loc(
            location,
            fragment_type,
          ) => [
            [%stri
              [%e
                make_error_expr(
                  Output_bucklescript_utils.conv_loc(location),
                  "Can't find fragment type: " ++ fragment_type,
                )
              ]
            ],
          ]
        }
      | errors => errors |> List.concat
      };
    };
  };
};

Bucklescript_config.read_config();

let get_module_bindings = structure => {
  structure
  |> filter_map(
       fun
       | {Parsetree.pstr_desc: Pstr_module(module_binding)} =>
         Some(module_binding)
       | _ => None,
     );
};

let get_module_declarations = signature => {
  signature
  |> filter_map(
       fun
       | {Parsetree.psig_desc: Psig_module(module_declaration)} =>
         Some(module_declaration)
       | _ => None,
     );
};

class mapper = {
  as self;
  inherit class Ast_traverse.map as super;
  pub! signature = sign => {
    sign
    |> List.fold_left(
         (acc, item) =>
           switch (item) {
           | {
               psig_desc:
                 Psig_module({
                   pmd_name: {txt: module_name},
                   pmd_type: {
                     pmty_desc:
                       Pmty_extension(({txt: "graphql", loc}, pstr)),
                   },
                 }),
             } =>
             switch (pstr) {
             | PStr([
                 {
                   pstr_desc:
                     Pstr_eval(
                       {
                         pexp_loc: loc,
                         pexp_desc:
                           Pexp_constant(Pconst_string(query, _, delim)),
                         _,
                       },
                       _,
                     ),
                   _,
                 },
               ]) =>
               List.append(
                 rewrite_definition_interface(
                   ~query_config=empty_query_config,
                   ~loc=conv_loc_from_ast(loc),
                   ~delim,
                   ~query,
                   ~module_name,
                   (),
                 )
                 |> List.rev,
                 acc,
               )
             | _ =>
               Location.raise_errorf(
                 ~loc,
                 "[%%graphql] accepts a string, e.g. [%%graphql {| { query |}]",
               )
             }
           | {psig_desc: Psig_recmodule(module_declarations)} as pstr => [
               {
                 ...pstr,
                 psig_desc:
                   Psig_recmodule(
                     module_declarations
                     |> List.fold_left(
                          acc =>
                            fun
                            | {
                                pmd_name: {txt: name},
                                pmd_type: {
                                  pmty_desc:
                                    Pmty_extension((
                                      {txt: "graphql", loc},
                                      pstr,
                                    )),
                                },
                              } => {
                                switch (pstr) {
                                | PStr([
                                    {
                                      pstr_desc:
                                        Pstr_eval(
                                          {
                                            pexp_loc: loc,
                                            pexp_desc:
                                              Pexp_constant(
                                                Pconst_string(
                                                  query,
                                                  _,
                                                  delim,
                                                ),
                                              ),
                                            _,
                                          },
                                          _,
                                        ),
                                      _,
                                    },
                                  ]) =>
                                  List.append(
                                    get_module_declarations(
                                      rewrite_definition_interface(
                                        ~query_config=empty_query_config,
                                        ~loc=conv_loc_from_ast(loc),
                                        ~delim,
                                        ~query,
                                        ~module_name=name,
                                        (),
                                      ),
                                    )
                                    |> List.rev,
                                    acc,
                                  )
                                | _ =>
                                  Location.raise_errorf(
                                    ~loc,
                                    "[%%graphql] accepts a string, e.g. [%%graphql {| { query |}]",
                                  )
                                };
                              }
                            | other => [other, ...acc],
                          [],
                        )
                     |> List.rev,
                   ),
               },
               ...acc,
             ]
           | other => [super#signature_item(other), ...acc]
           },
         [],
       )
    |> List.rev;
  };
  pub! structure = struc => {
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
                 Pstr_module({
                   pmb_name: {txt: _},
                   pmb_expr: {
                     pmod_desc:
                       Pmod_constraint(
                         {
                           pmod_desc:
                             Pmod_extension(({txt: "graphql", loc}, pstr)),
                           _,
                         },
                         _,
                       ),
                   },
                 }),
             } as item
           | {
               pstr_desc:
                 Pstr_eval(
                   {
                     pexp_desc: Pexp_extension(({txt: "graphql", loc}, pstr)),
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
                           Pexp_extension(({txt: "graphql", loc}, pstr)),
                       },
                     },
                   ],
                 ),
             } as item => {
               let module_name =
                 switch (item) {
                 | {pstr_desc: Pstr_module({pmb_name: {txt: name}})} => name
                 | _ => None
                 };

               let module_type =
                 switch (item) {
                 // when we specificly constrain a module with a graphql
                 // extension point. This is not a practical use-case,
                 //
                 //    module Bla: [%graphql {| ... |} = { ... }
                 //
                 // I'll leave it here, but might remove it later...
                 | {
                     pstr_desc:
                       Pstr_module({
                         pmb_expr: {
                           pmod_desc:
                             Pmod_constraint(
                               _,
                               {
                                 pmty_loc,
                                 pmty_attributes,
                                 pmty_desc:
                                   Pmty_extension((
                                     {txt: "graphql", _},
                                     PStr([
                                       {
                                         pstr_desc:
                                           Pstr_eval(
                                             {
                                               pexp_loc: loc,
                                               pexp_desc:
                                                 Pexp_constant(
                                                   Pconst_string(
                                                     query,
                                                     _,
                                                     delim,
                                                   ),
                                                 ),
                                               _,
                                             },
                                             _,
                                           ),
                                         _,
                                       },
                                     ]),
                                   )),
                               },
                             ),
                         },
                         _,
                       }),
                   } =>
                   Some({
                     pmty_loc,
                     pmty_attributes,
                     pmty_desc:
                       Pmty_signature(
                         rewrite_definition_interface(
                           ~query_config=empty_query_config,
                           ~loc=conv_loc_from_ast(loc),
                           ~delim,
                           ~query,
                           ~module_name=None,
                           (),
                         ),
                       ),
                   })
                 | {
                     pstr_desc:
                       Pstr_module({
                         pmb_expr: {
                           pmod_desc: Pmod_constraint(_, module_type),
                         },
                         _,
                       }),
                   } =>
                   Some(module_type)
                 | _ => None
                 };
               switch (pstr) {
               | PStr([
                   {
                     pstr_desc:
                       Pstr_eval(
                         {
                           pexp_loc: loc,
                           pexp_desc:
                             Pexp_constant(Pconst_string(query, _, delim)),
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
                   rewrite_definition(
                     ~query_config=
                       get_query_config_from_trailing_record(fields),
                     ~loc=conv_loc_from_ast(loc),
                     ~delim,
                     ~query,
                     ~module_name,
                     ~module_type,
                     (),
                   )
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
                             Pexp_constant(Pconst_string(query, _, delim)),
                           _,
                         },
                         _,
                       ),
                     _,
                   },
                 ]) =>
                 List.append(
                   rewrite_definition(
                     ~query_config=empty_query_config,
                     ~loc=conv_loc_from_ast(loc),
                     ~delim,
                     ~query,
                     ~module_name,
                     ~module_type,
                     (),
                   )
                   |> List.rev,
                   acc,
                 )
               | _ =>
                 Location.raise_errorf(
                   ~loc,
                   "[%%graphql] accepts a string, e.g. [%%graphql {| { query |}]",
                 )
               };
             }
           | {pstr_desc: Pstr_recmodule(module_bindings)} as pstr => [
               {
                 ...pstr,
                 pstr_desc:
                   Pstr_recmodule(
                     module_bindings
                     |> List.fold_left(
                          acc =>
                            fun
                            | {
                                pmb_name: {txt: name},
                                pmb_expr:
                                  {
                                    pmod_desc:
                                      Pmod_constraint(
                                        {
                                          pmod_desc:
                                            Pmod_extension((
                                              {txt: "graphql", loc},
                                              pstr,
                                            )),
                                          _,
                                        },
                                        _,
                                      ),
                                  } as module_expr,
                              }
                            | {
                                pmb_name: {txt: name},
                                pmb_expr:
                                  {
                                    pmod_desc:
                                      Pmod_extension((
                                        {txt: "graphql", loc},
                                        pstr,
                                      )),
                                  } as module_expr,
                              } => {
                                let module_name = name;
                                let module_type =
                                  switch (module_expr) {
                                  | {
                                      pmod_desc:
                                        Pmod_constraint(
                                          _,
                                          {
                                            pmty_desc:
                                              Pmty_signature(signature),
                                            pmty_loc,
                                            pmty_attributes,
                                          },
                                        ),
                                    } =>
                                    Some({
                                      pmty_desc:
                                        Pmty_signature(
                                          self#signature(signature),
                                        ),
                                      pmty_loc,
                                      pmty_attributes,
                                    })
                                  | {
                                      pmod_desc:
                                        Pmod_constraint(_, module_type),
                                    } =>
                                    Some(module_type)
                                  | _ => None
                                  };
                                switch (pstr) {
                                | PStr([
                                    {
                                      pstr_desc:
                                        Pstr_eval(
                                          {
                                            pexp_loc: loc,
                                            pexp_desc:
                                              Pexp_constant(
                                                Pconst_string(
                                                  query,
                                                  _,
                                                  delim,
                                                ),
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
                                          {
                                            pexp_desc:
                                              Pexp_record(fields, None),
                                            _,
                                          },
                                          _,
                                        ),
                                      _,
                                    },
                                  ]) =>
                                  List.append(
                                    get_module_bindings(
                                      rewrite_definition(
                                        ~query_config=
                                          get_query_config_from_trailing_record(
                                            fields,
                                          ),
                                        ~loc=conv_loc_from_ast(loc),
                                        ~delim,
                                        ~query,
                                        ~module_name,
                                        ~module_type,
                                        (),
                                      ),
                                    )
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
                                                Pconst_string(
                                                  query,
                                                  _,
                                                  delim,
                                                ),
                                              ),
                                            _,
                                          },
                                          _,
                                        ),
                                      _,
                                    },
                                  ]) =>
                                  List.append(
                                    get_module_bindings(
                                      rewrite_definition(
                                        ~query_config=empty_query_config,
                                        ~loc=conv_loc_from_ast(loc),
                                        ~delim,
                                        ~query,
                                        ~module_name,
                                        ~module_type,
                                        (),
                                      ),
                                    )
                                    |> List.rev,
                                    acc,
                                  )
                                | _ =>
                                  Location.raise_errorf(
                                    ~loc,
                                    "[%%graphql] accepts a string, e.g. [%%graphql {| { query |}]",
                                  )
                                };
                              }
                            | other => [other, ...acc],
                          [],
                        )
                     |> List.rev,
                   ),
               },
               ...acc,
             ]

           | other => [super#structure_item(other), ...acc],
         [],
       )
    |> List.rev;
  };
};

let structure_mapper = s => (new mapper)#structure(s);
let signature_mapper = s => (new mapper)#signature(s);

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
    "-future-added-value",
    Arg.Bool(
      future_added_value =>
        Ppx_config.update_config(current => {...current, future_added_value}),
    ),
    "Omits the `FutureAddedValue variant for enums if set to false",
  ),
  (
    "-verbose-errors",
    Arg.Unit(
      () =>
        Ppx_config.update_config(current =>
          {...current, verbose_error_handling: true}
        ),
    ),
    "Verbose error handling. If not defined NODE_ENV will be used",
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
    "the import to use for the template tag (default is \"default\")",
  ),
  (
    "-template-tag-location",
    Arg.String(
      template_tag_location =>
        Ppx_config.update_config(current =>
          {...current, template_tag_location: Some(template_tag_location)}
        ),
    ),
    "the import location for the template tag",
  ),
  (
    "-template-tag-return-type",
    Arg.String(
      template_tag_return_type =>
        Ppx_config.update_config(current =>
          {
            ...current,
            template_tag_return_type: Some(template_tag_return_type),
          }
        ),
    ),
    "the import location for the template tag",
  ),
  (
    "-fragment-in-query",
    Arg.String(
      fun
      | "exclude" =>
        Ppx_config.update_config(current =>
          {...current, fragment_in_query: Exclude}
        )
      | _ =>
        Ppx_config.update_config(current =>
          {...current, fragment_in_query: Include}
        ),
    ),
    "Include the full fragment in the query (either 'include' or 'exclude')",
  ),
  (
    "-extend-query",
    Arg.String(
      extend_query =>
        Ppx_config.update_config(current =>
          {...current, extend_query: Some(extend_query)}
        ),
    ),
    "extend queries with the following functor",
  ),
  (
    "-extend-query-no-required-variables",
    Arg.String(
      extend_query_no_required_variables =>
        Ppx_config.update_config(current =>
          {
            ...current,
            extend_query_no_required_variables:
              Some(extend_query_no_required_variables),
          }
        ),
    ),
    "extend queries having no required variables with the following functor",
  ),
  (
    "-extend-mutation",
    Arg.String(
      extend_mutation =>
        Ppx_config.update_config(current =>
          {...current, extend_mutation: Some(extend_mutation)}
        ),
    ),
    "extend mutations with the following functor",
  ),
  (
    "-extend-mutation-no-required-variables",
    Arg.String(
      extend_mutation_no_required_variables =>
        Ppx_config.update_config(current =>
          {
            ...current,
            extend_mutation_no_required_variables:
              Some(extend_mutation_no_required_variables),
          }
        ),
    ),
    "extend mutations having no required variables with the following functor",
  ),
  (
    "-extend-subscription",
    Arg.String(
      extend_subscription =>
        Ppx_config.update_config(current =>
          {...current, extend_subscription: Some(extend_subscription)}
        ),
    ),
    "extend subscriptions with the following functor",
  ),
  (
    "-extend-subscription-no-required-variables",
    Arg.String(
      extend_subscription_no_required_variables =>
        Ppx_config.update_config(current =>
          {
            ...current,
            extend_subscription_no_required_variables:
              Some(extend_subscription_no_required_variables),
          }
        ),
    ),
    "extend subscriptions having no required variables with the following functor",
  ),
  (
    "-extend-fragment",
    Arg.String(
      extend_fragment =>
        Ppx_config.update_config(current =>
          {...current, extend_fragment: Some(extend_fragment)}
        ),
    ),
    "extend fragments with the following functor",
  ),
  (
    "-native",
    Arg.Unit(
      () => Ppx_config.update_config(current => {...current, native: true}),
    ),
    "native mode (non-ReScript)",
  ),
];

let () = {
  List.iter(
    ((k, spec, doc)) => Ppxlib.Driver.add_arg(k, spec, ~doc),
    args,
  );
  Ppxlib.Driver.register_transformation(
    ~preprocess_impl=structure_mapper,
    ~preprocess_intf=signature_mapper,
    "graphql",
  );
};

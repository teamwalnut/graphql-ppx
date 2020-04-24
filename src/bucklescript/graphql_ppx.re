open Migrate_parsetree;
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

// get's the type of a field name
let unsafe_get_field_type = (schema, ty: Schema.type_meta, name) => {
  let ty_fields =
    switch (ty) {
    | Interface({im_fields, _}) => im_fields
    | Object({om_fields, _}) => om_fields
    | _ => []
    };
  Schema.(
    List.find(fm => fm.Schema.fm_name == name, ty_fields).fm_field_type
    |> Graphql_printer.type_ref_name
    |> Schema.lookup_type(schema)
    |> Option.unsafe_unwrap
  );
};
/**
 * This is a GraphQL AST transform that will add the __typename to all selection
 * sets. This is necessary for Apollo, as this is an implicit field.
 * If we don't include this when using Apollo, the __typename information is
 * lost when we parse values and then serialize back to the js-values again
 */
let rec add_typename_to_selection_set =
        (parent_span, schema, ty, selection_set) => {
  let add_typename =
    switch (ty, selection_set) {
    | (_, []) => false
    // do not add typename to a single fragment spread. It will be added in the
    // fragment anyway, and it will prevent the result to be a single record of
    // the type of the fragment
    | (_, [Graphql_ast.FragmentSpread(_)]) => false
    | (Schema.Interface(_), selection_set) => true
    | (Schema.Union(_), selection_set) => true
    | (Schema.Object({om_name}), selection_set) =>
      // do not ineject __typename in top-level subscription type
      if (schema.Schema.meta.sm_subscription_type == Some(om_name)) {
        false;
      } else {
        true;
      }
    | (_, selection_set) => false
    };

  if (add_typename) {
    [
      Graphql_ast.Field({
        span: parent_span,
        item: {
          fd_alias: None,
          fd_name: {
            span: parent_span,
            item: "__typename",
          },
          fd_arguments: None,
          fd_directives: [],
          fd_selection_set: None,
        },
      }),
      ...traverse_selection_set(
           schema,
           ty,
           selection_set,
           add_typename_to_selection_set,
         ),
    ];
  } else {
    traverse_selection_set(
      schema,
      ty,
      selection_set,
      add_typename_to_selection_set,
    );
  };
}
/**
 * This is a GraphQL AST transform that removes the `__typename` field from
 * unions. The PPX will add this in the printer stage, so it is always there.
 * The PPX will not allow any scalar fields on a union except for the inline
 * fragments, so we make sure to remove it before doing any further processing.
 */
and remove_typename_from_union = (parent_span, schema, ty, selection_set) => {
  let selection_set =
    switch (ty, selection_set) {
    | (Schema.Interface(_), selection_set)
    | (Schema.Union(_), selection_set) =>
      Graphql_ast.(
        selection_set
        |> List.fold_left(
             acc =>
               fun
               | Field({item: {fd_name: {item: "__typename"}}}) => acc
               | other => [other, ...acc],
             [],
           )
        |> List.rev
      )
    | (_, selection_set) => selection_set
    };
  traverse_selection_set(
    schema,
    ty,
    selection_set,
    remove_typename_from_union,
  );
}
and traverse_selection_set = (schema, ty, selection_set, fn) => {
  Graphql_ast.(
    selection_set
    |> List.map(
         fun
         | Graphql_ast.Field({
             span: field_span,
             item:
               {
                 fd_name,
                 fd_selection_set:
                   Some({item: selection_set, span: selection_set_span}),
               } as field,
           }) => {
             let field_ty = unsafe_get_field_type(schema, ty, fd_name.item);
             let selection_set =
               fn(selection_set_span, schema, field_ty, selection_set);

             Field({
               span: field_span,
               item: {
                 ...field,
                 fd_selection_set:
                   Some({item: selection_set, span: selection_set_span}),
               },
             });
           }
         | other => other,
       )
  );
};

let traverse_document_selections = (fn, schema: Schema.schema, definitions) => {
  Graphql_ast.(
    definitions
    |> List.map(def => {
         switch (def) {
         | Operation({item as op, span}) =>
           let ty_name =
             switch (op.o_type) {
             | Query => schema.meta.sm_query_type
             | Mutation => Option.unsafe_unwrap(schema.meta.sm_mutation_type)
             | Subscription =>
               Option.unsafe_unwrap(schema.meta.sm_subscription_type)
             };
           let ty =
             Schema.lookup_type(schema, ty_name) |> Option.unsafe_unwrap;

           Operation({
             span,
             item: {
               ...op,
               o_selection_set: {
                 item: fn(span, schema, ty, op.o_selection_set.item),
                 span: op.o_selection_set.span,
               },
             },
           });

         | Fragment({item as f, span}) =>
           let ty_name = f.fg_type_condition.item;
           let ty =
             Schema.lookup_type(schema, ty_name) |> Option.unsafe_unwrap;

           Fragment({
             item: {
               ...f,
               fg_selection_set: {
                 item: fn(span, schema, ty, f.fg_selection_set.item),
                 span: f.fg_selection_set.span,
               },
             },
             span,
           });
         }
       })
  );
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
    (
      ~query_config: query_config,
      ~loc,
      ~delim,
      ~query,
      ~module_definition,
      (),
    ) => {
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
            document
            |> traverse_document_selections(
                 add_typename_to_selection_set,
                 schema,
               );
          } else {
            document;
          }
        )
        |> traverse_document_selections(remove_typename_from_union, schema);
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
      | Some(errs) =>
        let errs =
          errs
          |> List.map(((loc, msg)) => {
               let loc = conv_loc(loc);
               %stri
               [%e make_error_expr(loc, msg)];
             });
        [errs];
      | None =>
        Result_decoder.unify_document_schema(config, document)
        |> Output_bucklescript_module.generate_modules(
             config,
             module_definition,
           )
      };
    };
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
      records: true,
      legacy: false,
      template_tag: None,
      template_tag_location: None,
      template_tag_import: None,
      definition: true,
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
                Ast_helper.(
                  Mod.mk(
                    Pmod_structure(
                      List.concat(
                        rewrite_query(
                          ~query_config=get_query_config(fields),
                          ~loc=conv_loc_from_ast(loc),
                          ~delim,
                          ~query,
                          ~module_definition=true,
                          (),
                        ),
                      ),
                    ),
                  )
                )
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
                Ast_helper.(
                  Mod.mk(
                    Pmod_structure(
                      List.concat(
                        rewrite_query(
                          ~query_config=empty_query_config,
                          ~loc=conv_loc_from_ast(loc),
                          ~delim,
                          ~query,
                          ~module_definition=true,
                          (),
                        ),
                      ),
                    ),
                  )
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
          structure: (mapper, struc) => {
            struc
            |> List.fold_left(
                 acc =>
                   fun
                   | {
                       pstr_desc:
                         Pstr_eval(
                           {
                             pexp_desc:
                               Pexp_extension(({txt: "graphql", loc}, pstr)),
                           },
                           _,
                         ),
                     }
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
                     } =>
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
                         acc,
                         List.concat(
                           rewrite_query(
                             ~query_config=get_query_config(fields),
                             ~loc=conv_loc_from_ast(loc),
                             ~delim,
                             ~query,
                             ~module_definition=false,
                             (),
                           ),
                         ),
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
                         acc,
                         List.concat(
                           rewrite_query(
                             ~query_config=empty_query_config,
                             ~loc=conv_loc_from_ast(loc),
                             ~delim,
                             ~query,
                             ~module_definition=false,
                             (),
                           ),
                         ),
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
                   | other =>
                     List.append(
                       acc,
                       [default_mapper.structure_item(mapper, other)],
                     ),
                 [],
               );
          },
        }
      )
    )
  );
};

let args = [
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

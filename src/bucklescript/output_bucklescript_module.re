open Migrate_parsetree;
open Graphql_ppx_base;
open Result_structure;
open Generator_utils;
open Ast_408;
open Asttypes;
open Parsetree;
open Ast_helper;
open Extract_type_definitions;
open Output_bucklescript_utils;

type operation_type = Graphql_ast.operation_type;
type operation_options = {has_required_variables: bool};

type definition =
  | Fragment
  | Operation(operation_type, operation_options);

module StringSet = Set.Make(String);
module VariableFinderImpl = {
  type t = ref(StringSet.t);
  let make_self = _ => ref(StringSet.empty);

  include Traversal_utils.AbstractVisitor;

  let enter_variable_value = (self, _, v) =>
    self := StringSet.add(v.Source_pos.item, self^);

  let from_self = (self: t): StringSet.t => self^;
};

module VariableFinder = Traversal_utils.Visitor(VariableFinderImpl);

let find_variables = (config, document) => {
  let ctx = Traversal_utils.make_context(config, document);
  VariableFinderImpl.from_self(VariableFinder.visit_document(ctx, document));
};

let pretty_print = (query: string): string => {
  let indent = ref(1);

  let str =
    query
    |> String.split_on_char('\n')
    |> List.map(l => String.trim(l))
    |> List.filter(l => l != "")
    |> List.map(line => {
         if (String.contains(line, '}')) {
           indent := indent^ - 1;
         };
         let line = String.make(indent^ * 2, ' ') ++ line;
         if (String.contains(line, '{')) {
           indent := indent^ + 1;
         };
         line;
       })
    |> String.concat("\n");

  str ++ "\n";
};

let compress_parts = (parts: array(Graphql_printer.t)) => {
  Graphql_printer.(
    parts
    |> Array.to_list
    |> List.fold_left(
         (acc, curr) => {
           switch (acc, curr) {
           | ([String(s1), ...rest], String(s2)) => [
               String(s1 ++ s2),
               ...rest,
             ]
           | (acc, curr) => [curr, ...acc]
           }
         },
         [],
       )
    |> List.rev
    |> Array.of_list
  );
};

let make_fragment_query = f => {
  Exp.ident({
    Location.txt: Longident.parse(f ++ ".query"),
    loc: Location.none,
  });
};

let emit_printed_template_query = (parts: array(Graphql_printer.t), config) => {
  open Graphql_printer;

  let fragment_query_refs =
    switch (config.fragment_in_query) {
    | Include =>
      parts
      |> Array.fold_left(
           acc =>
             fun
             | String(_) => acc
             | FragmentNameRef(_) => acc
             | FragmentQueryRef(f) => [f, ...acc],
           [],
         )
      |> List.rev
    | Exclude => []
    };

  let query =
    Array.fold_left(
      acc =>
        fun
        | String(s) => acc ++ s
        | FragmentNameRef(f) => {
            acc ++ f;
          }
        | FragmentQueryRef(_) => acc,
      "",
      parts,
    );
  let fragment_names =
    fragment_query_refs
    |> List.mapi((i, _frag) => "frag_" ++ string_of_int(i));
  let fragments = fragment_names |> List.map(name => {"${" ++ name ++ "}\n"});

  [query, ...fragments] |> List.fold_left((acc, el) => acc ++ el, "");
};

let emit_printed_query = (parts, config) => {
  open Graphql_printer;
  let make_string = s => {
    Exp.constant(Parsetree.Pconst_string(s, None));
  };
  let join = (part1, part2) => {
    Ast_helper.(
      Exp.apply(
        Exp.ident({Location.txt: Longident.parse("^"), loc: Location.none}),
        [(Nolabel, part1), (Nolabel, part2)],
      )
    );
  };

  let query =
    Array.fold_left(
      acc =>
        fun
        | String(s) => acc ++ s
        | FragmentNameRef(f) => acc ++ f
        | FragmentQueryRef(_) => acc,
      "",
      parts,
    );

  let fragment_query_refs =
    switch (config.fragment_in_query) {
    | Include =>
      parts
      |> Array.fold_left(
           acc =>
             fun
             | String(_) => acc
             | FragmentNameRef(_) => acc
             | FragmentQueryRef(f) => [make_fragment_query(f), ...acc],
           [],
         )
      |> List.rev
    | Exclude => []
    };

  fragment_query_refs
  |> List.fold_left((acc, el) => join(acc, el), make_string(query));
};

let rec emit_json = config =>
  Ast_408.(
    fun
    | `Assoc(vs) => {
        let pairs =
          Ast_helper.(
            Exp.array(
              vs
              |> List.map(((key, value)) =>
                   Exp.tuple([
                     Exp.constant(Pconst_string(key, None)),
                     emit_json(config, value),
                   ])
                 ),
            )
          );
        %expr
        Js.Json.object_(Js.Dict.fromArray([%e pairs]));
      }
    | `List(ls) => {
        let values = Ast_helper.Exp.array(List.map(emit_json(config), ls));
        %expr
        Js.Json.array([%e values]);
      }
    | `Bool(true) => [%expr Js.Json.boolean(true)]
    | `Bool(false) => [%expr Js.Json.boolean(false)]
    | `Null => [%expr Obj.magic(Js.Undefined.empty)]
    | `String(s) => [%expr
        Js.Json.string([%e Ast_helper.Exp.constant(Pconst_string(s, None))])
      ]
    | `Int(i) => [%expr
        Js.Json.number(
          [%e
            Ast_helper.Exp.constant(Pconst_float(string_of_int(i), None))
          ],
        )
      ]
    | `StringExpr(parts) => [%expr
        Js.Json.string([%e emit_printed_query(parts, config)])
      ]
  );

let wrap_template_tag = (~import=?, ~location=?, ~template_tag=?, source) => {
  switch (import, location, template_tag) {
  | (None, Some(location), _)
  | (Some("default"), Some(location), _) =>
    "require(\"" ++ location ++ "\")" ++ "`\n" ++ source ++ "`"
  | (Some(import), Some(location), _) =>
    "require(\"" ++ location ++ "\")." ++ import ++ "`\n" ++ source ++ "`"
  | (_, _, Some(template_tag)) => template_tag ++ "`\n" ++ source ++ "`"
  | _ => source
  };
};

let wrap_structure_raw = contents => {
  Str.extension((
    {txt: "raw", loc: Location.none},
    PStr([
      {
        pstr_desc:
          Pstr_eval(
            Exp.constant(Parsetree.Pconst_string(contents, None)),
            [],
          ),
        pstr_loc: Location.none,
      },
    ]),
  ));
};

let wrap_raw = contents => {
  Exp.extension((
    {txt: "raw", loc: Location.none},
    PStr([
      {
        pstr_desc:
          Pstr_eval(
            Exp.constant(Parsetree.Pconst_string(contents, None)),
            [],
          ),
        pstr_loc: Location.none,
      },
    ]),
  ));
};

let make_printed_query = (config, document) => {
  let source = Graphql_printer.print_document(config.schema, document);
  let reprinted =
    switch (Ppx_config.output_mode()) {
    | Ppx_config.Apollo_AST => (
        None,
        Ast_serializer_apollo.serialize_document(source, document)
        |> emit_json(config),
      )
    | Ppx_config.String =>
      switch (config.template_tag) {
      | (template_tag, location, import)
          when template_tag != None || location != None =>
        open Graphql_printer;
        // the only way to emit a template literal for now, using the bs.raw
        // extension
        let fragments =
          source
          |> Array.fold_left(
               acc =>
                 fun
                 | String(_) => acc
                 | FragmentNameRef(_) => acc
                 | FragmentQueryRef(f) => [f, ...acc],
               [],
             )
          |> List.rev;

        let template_tag =
          wrap_template_tag(
            ~template_tag?,
            ~location?,
            ~import?,
            pretty_print(emit_printed_template_query(source, config)),
          );

        (
          None,
          switch (fragments) {
          | [] => wrap_raw(template_tag)
          | fragments =>
            let fragment_names =
              fragments
              |> List.mapi((i, _frag) => "frag_" ++ string_of_int(i));

            let frag_fun =
              "("
              ++ (
                List.tl(fragment_names)
                |> List.fold_left(
                     (acc, el) => acc ++ ", " ++ el,
                     List.hd(fragment_names),
                   )
              )
              ++ ") => ";

            Exp.apply(
              wrap_raw(frag_fun ++ template_tag),
              fragments |> List.map(f => (Nolabel, make_fragment_query(f))),
            );
          },
        );

      | (_, _, _) => (None, emit_printed_query(source, config))
      }
    };

  reprinted;
};

let wrap_module = (~loc as _, name: string, contents) => {
  let loc = Location.none;
  {
    pstr_desc:
      Pstr_module({
        pmb_name: {
          txt: Generator_utils.capitalize_ascii(name),
          loc,
        },
        pmb_expr: Mod.structure(contents),
        pmb_attributes: [],
        pmb_loc: loc,
      }),
    pstr_loc: loc,
  };
};

let wrap_query_module =
    (~loc as _, definition, name: string, contents, config) => {
  let loc = Location.none;
  let module_name = Generator_utils.capitalize_ascii(name ++ "'");
  let funct =
    switch (config.extend) {
    | Some(funct) => Some(funct)
    | None =>
      switch (definition) {
      | Fragment => Ppx_config.extend_fragment()
      | Operation(Query, {has_required_variables: false}) =>
        switch (Ppx_config.extend_query_no_required_variables()) {
        | Some(extension) => Some(extension)
        | None => Ppx_config.extend_query()
        }
      | Operation(Query, _) => Ppx_config.extend_query()
      | Operation(Mutation, {has_required_variables: false}) =>
        switch (Ppx_config.extend_mutation_no_required_variables()) {
        | Some(extension) => Some(extension)
        | None => Ppx_config.extend_mutation()
        }
      | Operation(Mutation, _) => Ppx_config.extend_mutation()
      | Operation(Subscription, {has_required_variables: false}) =>
        switch (Ppx_config.extend_subscription_no_required_variables()) {
        | Some(extension) => Some(extension)
        | None => Ppx_config.extend_subscription()
        }
      | Operation(Subscription, _) => Ppx_config.extend_subscription()
      }
    };

  switch (funct) {
  | Some(funct) =>
    let inner_result = [
      Str.include_(
        Incl.mk(Mod.ident({txt: Longident.parse(module_name), loc})),
      ),
      Str.include_(
        Incl.mk(
          Mod.apply(
            Mod.ident({txt: Longident.parse(funct), loc}),
            Mod.ident({txt: Longident.parse(module_name), loc}),
          ),
        ),
      ),
    ];
    [
      wrap_module(~loc, module_name, contents),
      wrap_module(~loc, name, inner_result),
    ];
  | None => [wrap_module(~loc, name, contents)]
  };
};

let generate_default_operation =
    (config, variable_defs, has_error, operation, res_structure) => {
  Output_bucklescript_docstrings.reset();
  let parse_fn =
    Output_bucklescript_parser.generate_parser(
      config,
      [],
      Graphql_ast.Operation(operation),
      res_structure,
    );
  let serialize_fn =
    Output_bucklescript_serializer.generate_serializer(
      config,
      [],
      Graphql_ast.Operation(operation),
      None,
      res_structure,
    );
  let types =
    Output_bucklescript_types.generate_types(
      config,
      res_structure,
      false,
      None,
    );
  // Add to internal module
  Output_bucklescript_docstrings.for_operation(config, operation);
  let raw_types =
    Output_bucklescript_types.generate_types(
      config,
      res_structure,
      true,
      None,
    );
  let arg_types =
    Output_bucklescript_types.generate_arg_types(
      false,
      config,
      variable_defs,
    );
  let raw_arg_types =
    Output_bucklescript_types.generate_arg_types(true, config, variable_defs);
  let extracted_args = extract_args(config, variable_defs);
  let serialize_variable_functions =
    Output_bucklescript_serializer.generate_serialize_variables(
      config,
      extracted_args,
    );
  let has_required_variables = has_required_variables(extracted_args);

  let contents =
    if (has_error) {
      [[%stri let parse: Raw.t => t = value => [%e parse_fn]]];
    } else {
      let variable_constructors =
        Output_bucklescript_serializer.generate_variable_constructors(
          config,
          extracted_args,
        );

      let (pre_printed_query, printed_query) =
        make_printed_query(config, [Graphql_ast.Operation(operation)]);

      let legacy_make_with_variables = [%stri
        let makeWithVariables = variables => {
          "query": query,
          "variables": serializeVariables(variables),
          "parse": parse,
        }
      ];

      List.concat([
        List.concat([
          [[%stri [@ocaml.warning "-32"]]],
          [
            wrap_module(
              ~loc=Location.none,
              "Raw",
              List.append(raw_types, raw_arg_types),
            ),
          ],
          types,
          switch (pre_printed_query) {
          | Some(pre_printed_query) => [pre_printed_query]
          | None => []
          },
          [
            Output_bucklescript_docstrings.(
              make_let("query", printed_query, query_docstring)
            ),
          ],
          arg_types,
          [
            Output_bucklescript_docstrings.(
              make_let(
                "parse",
                [%expr (value: Raw.t) => ([%e parse_fn]: t)],
                parse_docstring,
              )
            ),
          ],
          [
            Output_bucklescript_docstrings.(
              make_let(
                "serialize",
                [%expr (value: t) => ([%e serialize_fn]: Raw.t)],
                serialize_docstring,
              )
            ),
          ],
          switch (serialize_variable_functions) {
          | None => []
          | Some(f) => [f]
          },
          switch (variable_constructors) {
          | None => [[%stri let makeVariables = () => ()]]
          | Some(c) => [c]
          },
          has_required_variables
            ? [] : [[%stri let makeDefaultVariables = () => makeVariables()]],
          config.legacy && variable_constructors != None
            ? [legacy_make_with_variables] : [],
          config.legacy && variable_constructors == None
            ? [
              [%stri
                let make = () => {
                  "query": query,
                  "variables": Js.Json.null,
                  "parse": parse,
                }
              ],
            ]
            : [],
          [
            [%stri external unsafe_fromJson: Js.Json.t => Raw.t = "%identity"],
          ],
          [[%stri external toJson: Raw.t => Js.Json.t = "%identity"]],
          [
            [%stri
              external variablesToJson: Raw.t_variables => Js.Json.t =
                "%identity"
            ],
          ],
          Output_bucklescript_docstrings.get_module(),
        ]),
      ]);
    };

  let name =
    switch (operation) {
    | {item: {o_name: Some({item: name})}} => Some(name)
    | _ => None
    };
  (
    Operation(
      operation.item.o_type,
      {has_required_variables: has_required_variables},
    ),
    name,
    contents,
    operation.span |> config.map_loc |> conv_loc,
  );
};

let generate_fragment_module =
    (config, name, required_variables, has_error, fragment, res_structure) => {
  Output_bucklescript_docstrings.reset();
  let parse_fn =
    Output_bucklescript_parser.generate_parser(
      config,
      [],
      Graphql_ast.Fragment(fragment),
      res_structure,
    );
  let serialize_fn =
    Output_bucklescript_serializer.generate_serializer(
      config,
      [],
      Graphql_ast.Fragment(fragment),
      None,
      res_structure,
    );
  let types =
    Output_bucklescript_types.generate_types(
      config,
      res_structure,
      false,
      Some((
        fragment.item.fg_type_condition.item,
        fragment.item.fg_name.span,
      )),
    );
  // Add to internal module
  Output_bucklescript_docstrings.for_fragment_root(config, fragment);
  let raw_types =
    Output_bucklescript_types.generate_types(
      config,
      res_structure,
      true,
      Some((
        fragment.item.fg_type_condition.item,
        fragment.item.fg_name.span,
      )),
    );

  let rec make_labeled_fun = body =>
    fun
    | [] => body
    | [(name, type_, span, type_span), ...tl] => {
        let loc = config.map_loc(span) |> conv_loc;
        let type_loc = config.map_loc(type_span) |> conv_loc;
        Ast_helper.(
          Exp.fun_(
            ~loc,
            Labelled(name),
            None,
            Pat.constraint_(
              Pat.var({txt: "_" ++ name, loc: type_loc}),
              Typ.variant(
                [
                  {
                    prf_desc:
                      Rtag(
                        {
                          txt:
                            Output_bucklescript_parser.type_name_to_words(
                              type_,
                            ),
                          loc: type_loc,
                        },
                        true,
                        [],
                      ),
                    prf_loc: type_loc,
                    prf_attributes: [],
                  },
                ],
                Closed,
                None,
              ),
            ),
            make_labeled_fun(body, tl),
          )
        );
      };

  let contents =
    if (has_error) {
      [[%stri let parse = (_vars, value) => [%e parse_fn]]];
    } else {
      let (pre_printed_query, printed_query) =
        make_printed_query(config, [Graphql_ast.Fragment(fragment)]);
      let verify_parse =
        make_labeled_fun(
          Exp.fun_(
            Labelled("fragmentName"),
            None,
            Pat.constraint_(
              Pat.var({txt: "_" ++ name, loc: Location.none}),
              Typ.variant(
                [
                  {
                    prf_desc: Rtag({txt: name, loc: Location.none}, true, []),
                    prf_loc: Location.none,
                    prf_attributes: [],
                  },
                ],
                Closed,
                None,
              ),
            ),
            [%expr (value: Raw.t) => parse(value)],
          ),
          required_variables,
        );
      // Add to internal module
      Output_bucklescript_docstrings.for_fragment(config, fragment);
      List.concat(
        List.concat([
          [
            [[%stri [@ocaml.warning "-32"]]],
            [wrap_module(~loc=Location.none, "Raw", raw_types)],
            types,
            switch (pre_printed_query) {
            | Some(pre_printed_query) => [pre_printed_query]
            | None => []
            },
            [
              Output_bucklescript_docstrings.(
                make_let("query", printed_query, query_docstring)
              ),
            ],
            [
              Output_bucklescript_docstrings.(
                make_let(
                  "parse",
                  [%expr (value: Raw.t) => ([%e parse_fn]: t)],
                  parse_docstring,
                )
              ),
            ],
            [@metaloc fragment.span |> config.map_loc |> conv_loc]
            [[%stri let verifyArgsAndParse = [%e verify_parse]]],
            [
              Output_bucklescript_docstrings.(
                make_let(
                  "serialize",
                  [%expr (value: t) => ([%e serialize_fn]: Raw.t)],
                  serialize_docstring,
                )
              ),
            ],
            [
              [%stri
                let verifyName = [%e
                  Ast_helper.(
                    Exp.function_([
                      Exp.case(
                        Pat.variant(name, None),
                        Exp.construct(
                          {txt: Longident.Lident("()"), loc: Location.none},
                          None,
                        ),
                      ),
                    ])
                  )
                ]
              ],
            ],
            [
              [%stri
                external unsafe_fromJson: Js.Json.t => Raw.t = "%identity"
              ],
            ],
            [[%stri external toJson: Raw.t => Js.Json.t = "%identity"]],
            Output_bucklescript_docstrings.get_module(),
          ],
        ]),
      );
    };

  (
    Fragment,
    Some(Generator_utils.capitalize_ascii(name)),
    contents,
    fragment.span |> config.map_loc |> conv_loc,
  );
};

let generate_definition = config =>
  fun
  | Def_operation(vdefs, has_error, operation, structure) =>
    generate_default_operation(config, vdefs, has_error, operation, structure)
  | Def_fragment(name, req_vars, has_error, fragment, structure) =>
    generate_fragment_module(
      config,
      name,
      req_vars,
      has_error,
      fragment,
      structure,
    );

let generate_modules = (config, module_name, operations) => {
  switch (operations) {
  | [] => []
  | [operation] =>
    switch (generate_definition(config, operation)) {
    | (definition, Some(name), contents, loc) =>
      config.inline
        ? [contents]
        : [
          wrap_query_module(
            ~loc,
            definition,
            switch (module_name) {
            | Some(name) => name
            | None => name
            },
            contents,
            config,
          ),
        ]
    | (definition, None, contents, loc) =>
      switch (module_name) {
      | Some(name) => [
          wrap_query_module(~loc, definition, name, contents, config),
        ]
      | None => [contents]
      }
    }
  | operations =>
    let contents =
      operations
      |> List.map(generate_definition(config))
      |> List.mapi((i, (definition, name, contents, loc)) =>
           switch (name) {
           | Some(name) =>
             wrap_query_module(~loc, definition, name, contents, config)
           | None =>
             wrap_query_module(
               ~loc,
               definition,
               "Untitled" ++ string_of_int(i),
               contents,
               config,
             )
           }
         );
    switch (module_name) {
    | Some(module_name) => [
        [
          wrap_module(~loc=Location.none, module_name, List.concat(contents)),
        ],
      ]
    | None => contents
    };
  };
};

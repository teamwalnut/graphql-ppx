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

type definition =
  | Fragment
  | Operation(operation_type);

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

let join = (part1, part2) => {
  Ast_helper.(
    Exp.apply(
      Exp.ident({Location.txt: Longident.parse("^"), loc: Location.none}),
      [(Nolabel, part1), (Nolabel, part2)],
    )
  );
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

let emit_printed_template_query = (parts: array(Graphql_printer.t)) => {
  switch (parts) {
  | [|String(s)|] => s
  | [||] => ""
  | parts =>
    Graphql_printer.(
      Array.fold_left(
        acc =>
          fun
          | String(s) => acc ++ s
          | FragmentNameRef(f) => {
              let name =
                switch (String.split_on_char('.', f) |> List.rev) {
                | [name, ..._] => name
                | [] => assert(false)
                };
              acc ++ name;
            }
          // This is the code to make the template tag compatible with Apollo
          // we can expose this as an option later. (we need to wait for new
          // %raw functionality to properly do template literals. So in current
          // state it is not possible to make it compatible with Apollo.
          // | FragmentQueryRef(f) => acc ++ "${" ++ f ++ ".query" ++ "}",
          | FragmentQueryRef(f) => acc,
        "",
        parts,
      )
    )
  };
};

let emit_printed_query = parts => {
  open Ast_408;
  let make_string = s => {
    Exp.constant(Parsetree.Pconst_string(s, None));
  };
  let make_fragment_name = f => {
    Exp.ident({
      Location.txt: Longident.parse(f ++ ".name"),
      loc: Location.none,
    });
  };
  let make_fragment_query = f => {
    Exp.ident({
      Location.txt: Longident.parse(f ++ ".query"),
      loc: Location.none,
    });
  };
  open Graphql_printer;
  let generate_expr = (acc, part) =>
    switch (acc, part) {
    | (None, String(s)) => Some(make_string(s))
    | (Some(acc), String(s)) => Some(join(acc, make_string(s)))
    | (None, FragmentNameRef(f)) => Some(make_fragment_name(f))
    | (Some(acc), FragmentNameRef(f)) =>
      Some(join(acc, make_fragment_name(f)))
    | (None, FragmentQueryRef(f)) => Some(make_fragment_query(f))
    | (Some(acc), FragmentQueryRef(f)) =>
      Some(join(acc, make_fragment_query(f)))
    };

  let result = parts |> Array.fold_left(generate_expr, None);

  switch (result) {
  | None => make_string("")
  | Some(e) => e
  };
};

let rec emit_json =
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
                     emit_json(value),
                   ])
                 ),
            )
          );
        %expr
        Js.Json.object_(Js.Dict.fromArray([%e pairs]));
      }
    | `List(ls) => {
        let values = Ast_helper.Exp.array(List.map(emit_json, ls));
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
        Js.Json.string([%e emit_printed_query(parts)])
      ]
  );

// we need to add a require statement because we cannot output the js value of
// a bucklescript identifier in the raw statement yet. This is a feature that
// should be coming, so then we don't need templateTagImport and templateTagLocatino
// anymore (and also this require statement will not be necessary anymore)
let pre_template_tag = (~location=?, ~import=?, template_tag) => {
  switch (import, location) {
  | (Some(import), Some(location)) =>
    Some(
      (
        switch (import, template_tag) {
        | ("default", template_tag) => "let " ++ template_tag
        | (import, template_tag) when import == template_tag =>
          "let { " ++ template_tag ++ " }"
        | (import, template_tag) =>
          "let { " ++ import ++ ": " ++ template_tag ++ " }"
        }
      )
      ++ " = "
      ++ "require(\""
      ++ location
      ++ "\")",
    )
  | _ => None
  };
};

let wrap_template_tag = (template_tag, source) => {
  // if the template literal is: "graphql"
  // a string is created like this: graphql`[query]`
  template_tag ++ "`\n" ++ source ++ "`";
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
        |> emit_json,
      )
    | Ppx_config.String =>
      Ast_408.(
        Ast_helper.(
          switch (config.template_tag) {
          | (None, _, _) => (None, emit_printed_query(source))
          | (Some(template_tag), location, import) =>
            // the only way to emit a template literal for now, using the bs.raw
            // extension
            (
              switch (pre_template_tag(~location?, ~import?, template_tag)) {
              | Some(contents) => Some(wrap_structure_raw(contents))
              | None => None
              },
              wrap_raw(
                wrap_template_tag(
                  template_tag,
                  pretty_print(emit_printed_template_query(source)),
                ),
              ),
            )
          }
        )
      )
    };

  reprinted;
};

let wrap_module = (name: string, contents) => {
  {
    pstr_desc:
      Pstr_module({
        pmb_name: {
          txt: Generator_utils.capitalize_ascii(name),
          loc: Location.none,
        },
        pmb_expr: Mod.structure(contents),
        pmb_attributes: [],
        pmb_loc: Location.none,
      }),
    pstr_loc: Location.none,
  };
};

let wrap_query_module = (definition, name: string, contents, config) => {
  let module_name = Generator_utils.capitalize_ascii(name ++ "'");
  let type_annotations = [
    (
      {txt: Longident.parse("t"), loc: Location.none},
      Typ.constr(
        {txt: Longident.parse(module_name ++ ".t"), loc: Location.none},
        [],
      ),
    ),
    (
      {txt: Longident.parse("Raw.t"), loc: Location.none},
      Typ.constr(
        {txt: Longident.parse(module_name ++ ".Raw.t"), loc: Location.none},
        [],
      ),
    ),
  ];
  let type_annotations =
    switch (definition) {
    | Fragment => type_annotations
    | Operation(_) =>
      List.append(
        [
          (
            {txt: Longident.parse("t_variables"), loc: Location.none},
            Typ.constr(
              {
                txt: Longident.parse(module_name ++ ".t_variables"),
                loc: Location.none,
              },
              [],
            ),
          ),
          (
            {txt: Longident.parse("Raw.t_variables"), loc: Location.none},
            Typ.constr(
              {
                txt: Longident.parse(module_name ++ ".Raw.t_variables"),
                loc: Location.none,
              },
              [],
            ),
          ),
        ],
        type_annotations,
      )
    };

  let inner_result = [
    Str.include_(
      Incl.mk(
        Mod.ident({txt: Longident.Lident(module_name), loc: Location.none}),
      ),
    ),
    [%stri
      let self: [%t
        Typ.package(
          {
            loc: Location.none,
            txt:
              Longident.parse(
                switch (definition) {
                | Fragment => "GraphQL_PPX.Fragment"
                | Operation(Query) => "GraphQL_PPX.Query"
                | Operation(Mutation) => "GraphQL_PPX.Mutation"
                | Operation(Subscription) => "GraphQL_PPX.Subscription"
                },
              ),
          },
          type_annotations,
        )
      ] = [%e
        Exp.pack(
          Mod.ident({
            txt:
              Longident.Lident(
                Generator_utils.capitalize_ascii(name ++ "'"),
              ),
            loc: Location.none,
          }),
        )
      ]
    ],
  ];

  let funct =
    switch (config.extend) {
    | Some(funct) => Some(funct)
    | None =>
      switch (definition) {
      | Fragment => Ppx_config.extend_fragment()
      | Operation(Query) => Ppx_config.extend_query()
      | Operation(Mutation) => Ppx_config.extend_mutation()
      | Operation(Subscription) => Ppx_config.extend_subscription()
      }
    };

  let inner_result =
    switch (funct) {
    | Some(funct) =>
      List.append(
        inner_result,
        [
          Str.include_(
            Incl.mk(
              Mod.apply(
                Mod.ident({
                  txt: Longident.Lident(funct),
                  loc: Location.none,
                }),
                Mod.ident({
                  txt:
                    Longident.Lident(
                      Generator_utils.capitalize_ascii(name ++ "'"),
                    ),
                  loc: Location.none,
                }),
              ),
            ),
          ),
        ],
      )
    | None => inner_result
    };

  [
    wrap_module(name ++ "'", contents),
    Ast_helper.(wrap_module(name, inner_result)),
  ];
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
          [wrap_module("Raw", List.append(raw_types, raw_arg_types))],
          switch (pre_printed_query) {
          | Some(pre_printed_query) => [pre_printed_query]
          | None => []
          },
          [[%stri let query = [%e printed_query]]],
          types,
          arg_types,
          [[%stri let parse: Raw.t => t = value => [%e parse_fn]]],
          [[%stri let serialize: t => Raw.t = value => [%e serialize_fn]]],
          switch (serialize_variable_functions) {
          | None => []
          | Some(f) => [f]
          },
          switch (variable_constructors) {
          | None => []
          | Some(c) => [c]
          },
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
          Output_bucklescript_docstrings.get_module(),
        ]),
      ]);
    };

  let name =
    switch (operation) {
    | {item: {o_name: Some({item: name})}} => Some(name)
    | _ => None
    };
  (Operation(operation.item.o_type), name, contents);
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
    | [] => [%expr ((value: Raw.t) => [%e body])]
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
      [[%stri let make = (_vars, value) => [%e parse_fn]]];
    } else {
      let (pre_printed_query, printed_query) =
        make_printed_query(config, [Graphql_ast.Fragment(fragment)]);
      let parse = [%stri
        let parse = [%e make_labeled_fun(parse_fn, required_variables)]
      ];
      // Add to internal module
      Output_bucklescript_docstrings.for_fragment(config, fragment);
      List.concat(
        List.concat([
          [
            switch (pre_printed_query) {
            | Some(pre_printed_query) => [pre_printed_query]
            | None => []
            },
            [[%stri let query = [%e printed_query]]],
            [wrap_module("Raw", raw_types)],
            types,
            [parse],
            [[%stri let serialize: t => Raw.t = value => [%e serialize_fn]]],
            [
              [%stri
                let name = [%e
                  Ast_helper.Exp.constant(Pconst_string(name, None))
                ]
              ],
            ],
            Output_bucklescript_docstrings.get_module(),
          ],
        ]),
      );
    };

  (Fragment, Some(Generator_utils.capitalize_ascii(name)), contents);
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
    | (definition, Some(name), contents) =>
      config.inline
        ? [contents]
        : [
          wrap_query_module(
            definition,
            switch (module_name) {
            | Some(name) => name
            | None => name
            },
            contents,
            config,
          ),
        ]
    | (definition, None, contents) =>
      switch (module_name) {
      | Some(name) => [
          wrap_query_module(definition, name, contents, config),
        ]
      | None => [contents]
      }
    }
  | operations =>
    let contents =
      operations
      |> List.map(generate_definition(config))
      |> List.mapi((i, (definition, name, contents)) =>
           switch (name) {
           | Some(name) =>
             wrap_query_module(definition, name, contents, config)
           | None =>
             wrap_query_module(
               definition,
               "Untitled" ++ string_of_int(i),
               contents,
               config,
             )
           }
         );
    switch (module_name) {
    | Some(module_name) => [
        [wrap_module(module_name, List.concat(contents))],
      ]
    | None => contents
    };
  };
};

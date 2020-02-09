open Migrate_parsetree;
open Graphql_ppx_base;
open Result_structure;
open Generator_utils;

open Ast_406;
open Asttypes;
open Parsetree;
open Ast_helper;
open Extract_type_definitions;
open Output_bucklescript_utils;

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

let emit_printed_query = parts => {
  open Ast_406;
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
    | (acc, Empty) => acc
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
  Ast_406.(
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

let make_printed_query = (config, document) => {
  let source = Graphql_printer.print_document(config.schema, document);
  let reprinted =
    switch (Ppx_config.output_mode()) {
    | Ppx_config.Apollo_AST =>
      Ast_serializer_apollo.serialize_document(source, document) |> emit_json
    | Ppx_config.String => emit_printed_query(source)
    };

  reprinted;
};

let generate_default_operation =
    (config, variable_defs, has_error, operation, res_structure) => {
  let parse_fn =
    Output_bucklescript_parser.generate_parser(
      config,
      [],
      Graphql_ast.Operation(operation),
      res_structure,
    );
  let types = Output_bucklescript_types.generate_types(config, res_structure);
  let arg_types =
    Output_bucklescript_types.generate_arg_types(config, variable_defs);
  let extracted_args = extract_args(config, variable_defs);
  let serialize_variable_functions =
    Output_bucklescript_serializer.generate_serialize_variables(
      config,
      extracted_args,
    );

  let contents =
    if (has_error) {
      [[%stri let parse = value => [%e parse_fn]]];
    } else {
      let variable_constructors =
        Output_bucklescript_serializer.generate_variable_constructors(
          config,
          extracted_args,
        );
      List.concat([
        List.concat([
          [
            [%stri
              let query = [%e
                make_printed_query(
                  config,
                  [Graphql_ast.Operation(operation)],
                )
              ]
            ],
          ],
          [[%stri type raw_t]],
          [types],
          switch (extracted_args) {
          | [] => []
          | _ => [arg_types]
          },
          [[%stri let parse: Js.Json.t => t = value => [%e parse_fn]]],
          switch (serialize_variable_functions) {
          | None => []
          | Some(f) => [f]
          },
          switch (variable_constructors, config.legacy, config.definition) {
          | (None, true, _)
          | (None, _, true) => [
              [%stri let makeVar = (~f, ()) => f(Js.Json.null)],
            ]
          | (None, _, _) => []
          | (Some(c), _, _) => [c]
          },
          config.legacy
            ? [
              [%stri
                let make =
                  makeVar(~f=variables => {
                    {"query": query, "variables": variables, "parse": parse}
                  })
              ],
              [%stri
                let makeWithVariables = variables => {
                  "query": query,
                  "variables": serializeVariables(variables),
                  "parse": parse,
                }
              ],
            ]
            : [],
          config.definition
            ? [[%stri let definition = (parse, query, makeVar)]] : [],
          switch (extracted_args) {
          | [] => []
          | _ => [[%stri let makeVariables = makeVar(~f=f => f)]]
          },
        ]),
      ]);
    };

  let name =
    switch (operation) {
    | {item: {o_name: Some({item: name})}} => Some(name)
    | _ => None
    };
  (name, contents);
};

let generate_fragment_module =
    (config, name, required_variables, has_error, fragment, res_structure) => {
  let parse_fn =
    Output_bucklescript_parser.generate_parser(
      config,
      [],
      Graphql_ast.Fragment(fragment),
      res_structure,
    );
  let types = Output_bucklescript_types.generate_types(config, res_structure);

  let rec make_labeled_fun = body =>
    fun
    | [] => [%expr ((value: Js.Json.t) => [%e body])]
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
                  Rtag(
                    {
                      txt:
                        Output_bucklescript_parser.type_name_to_words(type_),
                      loc: type_loc,
                    },
                    [],
                    true,
                    [],
                  ),
                ],
                Closed,
                None,
              ),
            ),
            make_labeled_fun(body, tl),
          )
        );
      };

  let query = make_printed_query(config, [Graphql_ast.Fragment(fragment)]);
  let parse =
    [@metaloc conv_loc(config.map_loc(fragment.span))]
    [%stri let parse = [%e make_labeled_fun(parse_fn, required_variables)]];

  let variable_names =
    find_variables(config, [Graphql_ast.Fragment(fragment)])
    |> StringSet.elements;

  let variable_obj_type =
    Typ.constr(
      {txt: Longident.Lident("t_variables"), loc: Location.none},
      [],
    );
  let contents =
    if (has_error) {
      [
        [%stri
          let make = (_vars: [%t variable_obj_type], value) => [%e parse_fn]
        ],
      ];
    } else {
      List.concat([
        [
          [%stri let query = [%e query]],
          types,
          [%stri type raw_t],
          Ast_helper.(
            Str.type_(
              Recursive,
              [
                Type.mk(
                  ~manifest=
                    Typ.constr(
                      {loc: Location.none, txt: Longident.Lident("t")},
                      [],
                    ),
                  {
                    loc: Location.none,
                    txt: "t_" ++ fragment.item.fg_type_condition.item,
                  },
                ),
              ],
            )
          ),
          parse,
          [%stri
            let name = [%e
              Ast_helper.Exp.constant(Pconst_string(name, None))
            ]
          ],
        ],
      ]);
    };

  (Some(Generator_utils.capitalize_ascii(name)), contents);
};

let wrap_module = (name: string, contents) => {
  [
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
    },
  ];
};

let generate_operation = config =>
  fun
  | Mod_default_operation(vdefs, has_error, operation, structure) =>
    generate_default_operation(config, vdefs, has_error, operation, structure)
  | Mod_fragment(name, req_vars, has_error, fragment, structure) =>
    generate_fragment_module(
      config,
      name,
      req_vars,
      has_error,
      fragment,
      structure,
    );

let generate_modules = (config, module_definition, operations) => {
  switch (operations) {
  | [] => []
  | [a] =>
    switch (generate_operation(config, a)) {
    | (Some(name), contents) =>
      config.inline || module_definition
        ? [contents] : [wrap_module(name, contents)]
    | (None, contents) => [contents]
    }
  | a =>
    a
    |> List.map(generate_operation(config))
    |> List.mapi((i, (name, contents)) =>
         switch (name) {
         | Some(name) => wrap_module(name, contents)
         | None => wrap_module("Untitled" ++ string_of_int(i), contents)
         }
       )
  };
};

open Graphql_ppx_base;
open Result_structure;
open Generator_utils;

open Ast_406;
open Asttypes;
open Parsetree;
open Ast_helper;
open Extract_type_definitions;

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

  [[%stri let query = [%e reprinted]]];
};

let generate_default_operation =
    (config, variable_defs, has_error, operation, res_structure) => {
  let parse_fn =
    Output_bucklescript_decoder.generate_decoder(config, res_structure);
  let types = Output_bucklescript_types.generate_types(res_structure);
  let arg_types =
    Output_bucklescript_types.generate_arg_types(config, variable_defs);
  let serialize_variable_functions =
    Output_bucklescript_encoder.generate_serialize_variables(
      extract_args(config, variable_defs),
    );
  if (has_error) {
    [[%stri let parse = value => [%e parse_fn]]];
  } else {
    let (rec_flag, encoders) =
      Output_bucklescript_encoder.generate_encoders(
        config,
        Result_structure.res_loc(res_structure),
        variable_defs,
      );
    let (make_fn, make_with_variables_fn, make_variables_fn, definition_tuple) =
      Output_bucklescript_unifier.make_make_fun(config, variable_defs);

    List.concat([
      make_printed_query(config, [Graphql_ast.Operation(operation)]),
      List.concat([
        [[%stri type raw_t]],
        [types],
        [arg_types],
        [[%stri let parse: Js.Json.t => t = value => [%e parse_fn]]],
        if (rec_flag == Recursive) {
          [
            {
              pstr_desc: Pstr_value(rec_flag, encoders |> Array.to_list),
              pstr_loc: Location.none,
            },
          ];
        } else {
          encoders
          |> Array.map(encoder =>
               {
                 pstr_desc: Pstr_value(Nonrecursive, [encoder]),
                 pstr_loc: Location.none,
               }
             )
          |> Array.to_list;
        },
        [serialize_variable_functions],
        [
          [%stri let make = [%e make_fn]],
          [%stri let makeWithVariables = [%e make_with_variables_fn]],
          [%stri let makeVariables = [%e make_variables_fn]],
          [%stri let definition = [%e definition_tuple]],
        ],
      ]),
    ]);
  };
};

let generate_fragment_module =
    (config, name, _required_variables, has_error, fragment, res_structure) => {
  let parse_fn =
    Output_bucklescript_decoder.generate_decoder(config, res_structure);
  let types = Output_bucklescript_types.generate_types(res_structure);

  let variable_names =
    find_variables(config, [Graphql_ast.Fragment(fragment)])
    |> StringSet.elements;

  let variable_fields =
    variable_names
    |> List.map(name =>
         Otag(
           {txt: name, loc: Location.none},
           [],
           Ast_helper.Typ.constr(
             {txt: Longident.Lident("unit"), loc: Location.none},
             [],
           ),
         )
       );
  let variable_obj_type =
    Ast_helper.Typ.constr(
      {txt: Longident.parse("Js.t"), loc: Location.none},
      [Ast_helper.Typ.object_(variable_fields, Open)],
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
        make_printed_query(config, [Graphql_ast.Fragment(fragment)]),
        [
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
          [%stri let parse: Js.Json.t => t = value => [%e parse_fn]],
          [%stri
            let name = [%e
              Ast_helper.Exp.constant(Pconst_string(name, None))
            ]
          ],
        ],
      ]);
    };

  let m =
    Pstr_module({
      pmb_name: {
        txt: Generator_utils.capitalize_ascii(name),
        loc: Location.none,
      },
      pmb_expr: Mod.structure(contents),
      pmb_attributes: [],
      pmb_loc: Location.none,
    });

  [{pstr_desc: m, pstr_loc: Location.none}];
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

let generate_modules = (config, operations) => {
  let generated = List.map(generate_operation(config), operations);
  Mod.mk(Pmod_structure(List.concat(generated)));
};

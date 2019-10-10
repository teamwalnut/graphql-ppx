open Graphql_ppx_base;
open Result_structure;
open Generator_utils;

open Ast_406;
open Asttypes;
open Parsetree;
open Ast_helper;

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

let ret_type_magic = [
  /* Some functor magic to determine the return type of parse */
  [%stri module type mt_ret = {type t;}],
  [%stri type typed_ret('a) = (module mt_ret with type t = 'a)],
  [%stri
    let ret_type = (type a, f: _ => a): typed_ret(a) => {
      module MT_Ret = {
        type t = a;
      };
      (module MT_Ret);
    }
  ],
  [%stri module MT_Ret = (val ret_type(parse))],
  [%stri type t = MT_Ret.t],
];

let emit_printed_query = parts => {
  open Ast_406;
  open Graphql_printer;
  let generate_expr = acc =>
    fun
    | Empty => acc
    | String(s) =>
      Ast_helper.(
        Exp.apply(
          Exp.ident({
            Location.txt: Longident.parse("^"),
            loc: Location.none,
          }),
          [
            (Nolabel, acc),
            (Nolabel, Exp.constant(Parsetree.Pconst_string(s, None))),
          ],
        )
      )
    | FragmentNameRef(f) =>
      Ast_helper.(
        Exp.apply(
          Exp.ident({
            Location.txt: Longident.parse("^"),
            loc: Location.none,
          }),
          [
            (Nolabel, acc),
            (
              Nolabel,
              Exp.ident({
                Location.txt: Longident.parse(f ++ ".name"),
                loc: Location.none,
              }),
            ),
          ],
        )
      )
    | FragmentQueryRef(f) =>
      Ast_helper.(
        Exp.apply(
          Exp.ident({
            Location.txt: Longident.parse("^"),
            loc: Location.none,
          }),
          [
            (Nolabel, acc),
            (
              Nolabel,
              Exp.ident({
                Location.txt: Longident.parse(f ++ ".query"),
                loc: Location.none,
              }),
            ),
          ],
        )
      );

  Array.fold_left(
    generate_expr,
    Ast_406.(Ast_helper.Exp.constant(Parsetree.Pconst_string("", None))),
    parts,
  );
};

let rec emit_json =
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
        [%e Ast_helper.Exp.constant(Pconst_float(string_of_int(i), None))],
      )
    ]
  | `StringExpr(parts) => [%expr
      Js.Json.string([%e emit_printed_query(parts)])
    ];

let make_printed_query = (config, document) => {
  let source = Graphql_printer.print_document(config.schema, document);
  let reprinted =
    switch (Ppx_config.output_mode()) {
    | Ppx_config.Apollo_AST =>
      Ast_serializer_apollo.serialize_document(source, document) |> emit_json
    | Ppx_config.String => emit_printed_query(source)
    };

  [
    [%stri let ppx_printed_query = [%e reprinted]],
    [%stri let query = ppx_printed_query],
  ];
};

let generate_default_operation =
    (config, variable_defs, has_error, operation, res_structure) => {
  let parse_fn =
    Output_bucklescript_decoder.generate_decoder(config, res_structure);

  if (has_error) {
    [[%stri let parse = value => [%e parse_fn]]];
  } else {
    let (rec_flag, encoders) =
      Output_bucklescript_variables_encoder.generate_encoders(
        config,
        Result_structure.res_loc(res_structure),
        variable_defs,
      );

    let (make_fn, make_with_variables_fn) =
      Output_bucklescript_unifier.make_make_fun(config, variable_defs);

    let serialize = () => {
      let serialize_fn =
        Output_bucklescript_query_encoder.generate_encoder(
          config,
          res_structure,
        );

      [%stri let serialize = [%e serialize_fn]];
    };

    let encoders =
      switch (rec_flag) {
      | Recursive => [
          {
            pstr_desc: Pstr_value(rec_flag, encoders),
            pstr_loc: Location.none,
          },
        ]
      | _ =>
        encoders
        |> List.map(encoder =>
             {
               pstr_desc: Pstr_value(Nonrecursive, [encoder]),
               pstr_loc: Location.none,
             }
           )
      };

    List.concat([
      make_printed_query(config, [Graphql_ast.Operation(operation)]),
      [[%stri let parse = value => [%e parse_fn]]],
      Ppx_config.serialization_experimental() ? [serialize()] : [],
      encoders,
      [
        [%stri let make = [%e make_fn]],
        [%stri let makeWithVariables = [%e make_with_variables_fn]],
      ],
      ret_type_magic,
    ]);
  };
};

let generate_fragment_module =
    (config, name, _required_variables, has_error, fragment, res_structure) => {
  let parse_fn =
    Output_bucklescript_decoder.generate_decoder(config, res_structure);

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

  let serialize = () => {
    let serialize_fn =
      Output_bucklescript_query_encoder.generate_encoder(
        config,
        res_structure,
      );

    [%stri let serialize = [%e serialize_fn]];
  };

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
        [[%stri let parse = value => [%e parse_fn]]],
        Ppx_config.serialization_experimental() ? [serialize()] : [],
        [
          [%stri
            let name = [%e
              Ast_helper.Exp.constant(Pconst_string(name, None))
            ]
          ],
        ],
        ret_type_magic,
      ]);
    };

  let pstr_desc =
    Pstr_module({
      pmb_name: {
        txt: Generator_utils.capitalize_ascii(name),
        loc: Location.none,
      },
      pmb_expr: Mod.structure(contents),
      pmb_attributes: [],
      pmb_loc: Location.none,
    });

  [{pstr_desc, pstr_loc: Location.none}];
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

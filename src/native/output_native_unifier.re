open Graphql_ppx_base;
open Graphql_ast;
open Source_pos;
open Generator_utils;

open Ast_406;
open Parsetree;
open Asttypes;

open Type_utils;
open Output_native_utils;

exception Unimplemented(string);

let make_make_fun = (config, variable_defs) => {
  let make_make_triple = (_loc, variables) => [%expr
    {
      as _;
      pub query = ppx_printed_query;
      pub variables = [%e variables];
      pub parse = parse
    }
  ];

  switch (variable_defs) {
  | Some({item, span}) =>
    let rec make_labelled_function = (defs, body) =>
      switch (defs) {
      | [] =>
        [@metaloc config.map_loc(span) |> conv_loc] [%expr (() => [%e body])]
      | [(name, def), ...tl] =>
        let name_loc = config.map_loc(name.span) |> conv_loc;
        Ast_helper.(
          Exp.fun_(
            ~loc=name_loc,
            switch (def.vd_type.item) {
            | Tr_non_null_list(_)
            | Tr_non_null_named(_) => Labelled(name.item)
            | Tr_list(_)
            | Tr_named(_) => Optional(name.item)
            },
            None,
            Pat.var(~loc=name_loc, {txt: name.item, loc: name_loc}),
            make_labelled_function(tl, body),
          )
        );
      };

    let make_object_function = (defs, body) => {
      let rec generate_bindings = defs =>
        switch (defs) {
        | [] => body
        | [(name, _), ...tl] =>
          let name_loc = config.map_loc(name.span) |> conv_loc;
          Ast_helper.Exp.let_(
            ~loc=name_loc,
            Nonrecursive,
            [
              Ast_helper.(
                Vb.mk(
                  ~loc=name_loc,
                  Pat.var(~loc=name_loc, {txt: name.item, loc: name_loc}),
                  Exp.send(
                    [%expr variables],
                    {txt: name.item, loc: name_loc},
                  ),
                )
              ),
            ],
            generate_bindings(tl),
          );
        };

      %expr
      (variables => [%e generate_bindings(defs)]);
    };

    let make_var_ctor = defs =>
      defs
      |> List.map(((name, def)) => {
           let parser_ =
             Output_native_encoder.parser_for_type(
               config.schema,
               config.map_loc(name.span),
               to_native_type_ref(to_schema_type_ref(def.vd_type.item)),
             );
           let loc = config.map_loc(name.span) |> conv_loc;
           [@metaloc loc]
           [%expr
             (
               [%e
                 Ast_helper.Exp.constant(
                   ~loc,
                   Pconst_string(name.item, None),
                 )
               ],
               [%e parser_](
                 [%e
                   Ast_helper.Exp.ident(
                     ~loc,
                     {txt: Longident.parse(name.item), loc},
                   )
                 ],
               ),
             )
           ];
         })
      |> Ast_helper.Exp.array;
    let loc = config.map_loc(span) |> conv_loc;
    let variable_ctor_body =
      [@metaloc loc]
      [%expr `Assoc([%e make_var_ctor(item)] |> Array.to_list)];

    (
      make_labelled_function(item, make_make_triple(loc, variable_ctor_body)),
      make_object_function(item, make_make_triple(loc, variable_ctor_body)),
    );
  | None => (
      [%expr (() => [%e make_make_triple(Location.none, [%expr `Null])])],
      [%expr
        ((_: {.}) => [%e make_make_triple(Location.none, [%expr `Null])])
      ],
    )
  };
};

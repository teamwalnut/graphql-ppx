open Graphql_ppx_base;
open Graphql_ast;
open Source_pos;
open Generator_utils;

open Ast_402;
open Parsetree;
open Asttypes;

open Type_utils;
open Output_bucklescript_utils;

exception Unimplemented(string);

let make_make_fun = (config, variable_defs) => {
  let make_make_triple = (loc, variables) =>
    Ast_helper.Exp.extension(
      ~loc,
      (
        {txt: "bs.obj", loc},
        PStr([
          [@metaloc loc]
          [%stri
            {query: ppx_printed_query, variables: [%e variables], parse}
          ],
        ]),
      ),
    );
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
            | Tr_non_null_named(_) => name.item
            | Tr_list(_)
            | Tr_named(_) => "?" ++ name.item
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
                  [%expr
                    variables##[%e
                                 Exp.ident({
                                   txt: Longident.Lident(name.item),
                                   loc: name_loc,
                                 })
                               ]
                  ],
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
             Output_bucklescript_variables_encoder.parser_for_type(
               config.schema,
               config.map_loc(name.span),
               to_native_type_ref(to_schema_type_ref(def.vd_type.item)),
             );
           let loc = config.map_loc(name.span) |> conv_loc;
           [@metaloc loc]
           [%expr
             (
               [%e
                 Ast_helper.Exp.constant(~loc, Const_string(name.item, None))
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
      [%expr
        Js.Json.object_(
          [%e make_var_ctor(item)]
          |> [%e filter_out_null_values]
          |> Js.Dict.fromArray,
        )
      ];

    (
      make_labelled_function(item, make_make_triple(loc, variable_ctor_body)),
      make_object_function(item, make_make_triple(loc, variable_ctor_body)),
    );
  | None => (
      [%expr
        (() => [%e make_make_triple(Location.none, [%expr Js.Json.null])])
      ],
      [%expr
        (
          (_: Js.t({.})) => [%e
            make_make_triple(Location.none, [%expr Js.Json.null])
          ]
        )
      ],
    )
  };
};

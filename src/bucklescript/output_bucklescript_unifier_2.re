open Graphql_ppx_base;
open Graphql_ast;
open Source_pos;
open Generator_utils;

open Ast_406;
open Parsetree;
open Asttypes;

open Type_utils;
open Output_bucklescript_utils;

exception Unimplemented(string);

let make_definition = (config, variable_defs) => {
  let make_tuple = (loc, variables, compose) => [%expr
    (
      parse,
      ppx_printed_query,
      graphql_ppx_use_json_variables_fn => [%e compose],
    )
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

    let make_var_ctor = defs =>
      defs
      |> List.map(((name, def)) => {
           let parser_ =
             Output_bucklescript_encoder.parser_for_type(
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
      [%expr
        Js.Json.object_(
          [%e make_var_ctor(item)]
          |> [%e Output_bucklescript_encoder.filter_out_null_values]
          |> Js.Dict.fromArray,
        )
      ];

    let user_function =
      make_labelled_function(
        item,
        [%expr graphql_ppx_use_json_variables_fn([%e variable_ctor_body])],
      );

    make_tuple(loc, variable_ctor_body, user_function);
  | None =>
    %expr
    (() => [%e make_tuple(Location.none, [%expr Js.Json.null], [%expr 0])])
  };
};

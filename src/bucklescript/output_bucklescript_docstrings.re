open Graphql_ppx_base;
open Output_bucklescript_utils;

open Migrate_parsetree;
open Ast_408;
open Ast_helper;

let for_fragment =
    (
      config: Generator_utils.output_config,
      fragment: Source_pos.spanning(Graphql_ast.fragment),
    ) => {
  switch (
    config.schema
    |> Schema_printer.print_type(fragment.item.fg_type_condition.item)
  ) {
  | None => []
  | Some(printed_type) =>
    let loc = conv_loc(config.map_loc(fragment.item.fg_type_condition.span));
    let doc_string =
      Docstrings.docstring("```\n" ++ printed_type ++ "\n```", loc);

    [
      [%stri type graphql],
      Str.value(
        Nonrecursive,
        [
          Vb.mk(
            ~attrs=[Docstrings.docs_attr(doc_string)],
            Ast_helper.Pat.var(Location.mkloc("_", loc)),
            Exp.constraint_(
              Exp.apply(
                Exp.ident(Location.mknoloc(Longident.parse("Obj.magic"))),
                [(Nolabel, Exp.constant(Pconst_integer("0", None)))],
              ),
              Typ.constr(Location.mknoloc(Longident.parse("graphql")), []),
            ),
          ),
        ],
      ),
    ];
  };
};

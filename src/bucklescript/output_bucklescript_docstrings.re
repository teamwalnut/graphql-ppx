open Graphql_ppx_base;
open Output_bucklescript_utils;

open Migrate_parsetree;
open Ast_408;
open Ast_helper;

let str_items: ref(list(Parsetree.structure_item)) = ref([]);

let reset = () => {
  str_items := [];
};

let for_root_identifier = (config: Generator_utils.output_config, loc) => {
  str_items :=
    List.append(
      str_items^,
      [
        Ast_helper.Str.type_(
          Recursive,
          [
            Type.mk(
              ~manifest=
                Typ.constr(Location.mkloc(Longident.parse("t"), loc), []),
              Location.mknoloc("root"),
            ),
          ],
        ),
      ],
    );
};

let for_operation =
    (
      config: Generator_utils.output_config,
      operation: Source_pos.spanning(Graphql_ast.operation),
    ) => {
  for_root_identifier(
    config,
    Output_bucklescript_utils.extend_loc_from_start(
      conv_loc(config.map_loc(operation.span)),
      switch (operation.item.o_type) {
      | Query => 5
      | Mutation => 8
      | Subscription => 12
      },
    ),
  );
};

let for_fragment_root =
    (
      config: Generator_utils.output_config,
      fragment: Source_pos.spanning(Graphql_ast.fragment),
    ) => {
  for_root_identifier(
    config,
    Output_bucklescript_utils.extend_loc_from_start(
      conv_loc(config.map_loc(fragment.span)),
      8,
    ),
  );
};

let for_input_constraint =
    (
      config: Generator_utils.output_config,
      Extract_type_definitions.InputField(field),
    ) => {
  switch (
    field.loc_type,
    Extract_type_definitions.get_inner_type(field.type_)
    |> Option.flat_map(innerType => {
         switch (innerType) {
         | Extract_type_definitions.Type(Object(_) as ty)
         | Extract_type_definitions.Type(Enum(_) as ty)
         | Extract_type_definitions.Type(Interface(_) as ty)
         | Extract_type_definitions.Type(Union(_) as ty)
         | Extract_type_definitions.Type(InputObject(_) as ty) =>
           let type_name = ty |> Schema.extract_name_from_type_meta;
           config.schema
           |> Schema_printer.print_type(type_name)
           |> Option.map(printed_type => (type_name, printed_type));
         | _ => None
         }
       }),
  ) {
  | (Some(loc_type), Some((type_name, printed_type))) =>
    let safe_name =
      "_graphql_"
      ++ type_name
      ++ "_"
      ++ (loc_type.loc_start.pos_cnum |> string_of_int);
    str_items :=
      List.append(
        str_items^,
        Ast_helper.[
          Str.type_(Nonrecursive, [Type.mk(Location.mknoloc(safe_name))]),
          Str.value(
            Nonrecursive,
            [
              Vb.mk(
                ~attrs=[
                  Docstrings.docs_attr(
                    Docstrings.docstring(
                      "```\n" ++ printed_type ++ "\n```",
                      conv_loc(loc_type),
                    ),
                  ),
                ],
                Ast_helper.Pat.var(
                  Location.mkloc(safe_name, conv_loc(loc_type)),
                ),
                Exp.constraint_(
                  Exp.apply(
                    Exp.ident(
                      Location.mknoloc(Longident.parse("Obj.magic")),
                    ),
                    [(Nolabel, Exp.constant(Pconst_integer("0", None)))],
                  ),
                  Typ.constr(
                    Location.mknoloc(Longident.parse(safe_name)),
                    [],
                  ),
                ),
              ),
            ],
          ),
        ],
      );
  | (None, _)
  | (_, None) => ()
  };
};

let for_fragment =
    (
      config: Generator_utils.output_config,
      fragment: Source_pos.spanning(Graphql_ast.fragment),
    ) => {
  switch (
    config.schema
    |> Schema_printer.print_type(fragment.item.fg_type_condition.item)
  ) {
  | None => ()
  | Some(printed_type) =>
    let loc = conv_loc(config.map_loc(fragment.item.fg_type_condition.span));
    let doc_string =
      Docstrings.docstring("```\n" ++ printed_type ++ "\n```", loc);

    str_items :=
      List.append(
        str_items^,
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
                    Exp.ident(
                      Location.mknoloc(Longident.parse("Obj.magic")),
                    ),
                    [(Nolabel, Exp.constant(Pconst_integer("0", None)))],
                  ),
                  Typ.constr(
                    Location.mknoloc(Longident.parse("graphql")),
                    [],
                  ),
                ),
              ),
            ],
          ),
        ],
      );
  };
};

let get_module = () => {
  switch (str_items^) {
  | [] => []
  | items => [
      Str.module_(
        Mb.mk(
          Location.mknoloc("Z__INTERNAL"),
          Mod.mk(Pmod_structure(items)),
        ),
      ),
    ]
  };
};

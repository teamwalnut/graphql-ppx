open Graphql_ppx_base;
open Output_bucklescript_utils;
open Graphql_ast;

open Migrate_parsetree;
open Ast_408;
open Ast_helper;

let str_items: ref(list(Parsetree.structure_item)) = ref([]);
let str_module_information: ref(list(Parsetree.structure_item)) = ref([]);

let reset = () => {
  str_items := [];
};

let generate_text_struct = (name, loc, text) => [
  Str.type_(Nonrecursive, [Type.mk(Location.mknoloc(name))]),
  Str.value(
    Nonrecursive,
    [
      Vb.mk(
        ~attrs=[
          Docstrings.docs_attr(Docstrings.docstring(text, conv_loc(loc))),
        ],
        Ast_helper.Pat.var(Location.mkloc(name, conv_loc(loc))),
        Exp.constraint_(
          Exp.apply(
            Exp.ident(Location.mknoloc(Longident.parse("Obj.magic"))),
            [(Nolabel, Exp.constant(Pconst_integer("0", None)))],
          ),
          Typ.constr(Location.mknoloc(Longident.parse(name)), []),
        ),
      ),
    ],
  ),
];

let for_field_arguments =
    (
      config: Generator_utils.output_config,
      field_meta: option(Schema.field_meta),
      arguments: Graphql_ast.arguments,
    ) => {
  switch (field_meta) {
  | None => ()
  | Some({fm_arguments, fm_name}) =>
    str_items :=
      List.concat([
        str_items^,
        ...arguments
           |> List.map(((name, type_): Graphql_ast.argument) => {
                List.concat([
                  switch (type_.item) {
                  | Iv_variable(var_name) =>
                    config.full_document
                    |> List.fold_left(
                         (p, def) => {
                           switch (def) {
                           | Fragment(_)
                           | Operation({
                               item: {o_variable_definitions: None},
                             })
                           | Operation({
                               item: {
                                 o_variable_definitions: Some({item: []}),
                               },
                             }) => p
                           | Operation({
                               item: {
                                 o_variable_definitions:
                                   Some({item as variable_definitions}),
                               },
                             }) =>
                             switch (
                               variable_definitions
                               |> List.find_opt(
                                    (
                                      (
                                        s_var_name:
                                          Source_pos.spanning(string),
                                        _,
                                      ),
                                    ) => {
                                    s_var_name.item == var_name
                                  })
                             ) {
                             | None => p
                             | Some((
                                 s_var_name,
                                 {vd_type: {item as s_type}},
                               )) =>
                               let loc = config.map_loc(type_.span);
                               let safe_name =
                                 "_graphql_"
                                 ++ var_name
                                 ++ "_"
                                 ++ (loc.loc_start.pos_cnum |> string_of_int);
                               List.append(
                                 p,
                                 generate_text_struct(
                                   safe_name,
                                   loc,
                                   "Variable **$"
                                   ++ var_name
                                   ++ "** has the following graphql type:\n\n```\n"
                                   ++ Schema_printer.print_type_from_ref(
                                        s_type |> Type_utils.to_schema_type_ref,
                                        config.schema,
                                      )
                                   ++ "\n```",
                                 ),
                               );
                             }
                           }
                         },
                         [],
                       )
                  | _ => []
                  },
                  switch (
                    fm_arguments
                    |> List.find_opt(({am_name}: Schema.argument_meta) => {
                         am_name == name.item
                       })
                  ) {
                  | None => []
                  | Some({am_arg_type}) =>
                    let name_loc = config.map_loc(name.span);
                    let safe_name =
                      "_graphql_"
                      ++ name.item
                      ++ "_"
                      ++ (name_loc.loc_start.pos_cnum |> string_of_int);
                    generate_text_struct(
                      safe_name,
                      name_loc,
                      "Argument **"
                      ++ name.item
                      ++ "** on field **"
                      ++ fm_name
                      ++ "** has the following graphql type:\n\n```\n"
                      ++ Schema_printer.print_type_from_ref(
                           am_arg_type,
                           config.schema,
                         )
                      ++ "\n```",
                    );
                  },
                ])
              }),
      ])
  };
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
        generate_text_struct(
          safe_name,
          loc_type,
          "```\n" ++ printed_type ++ "\n```",
        ),
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
    let loc = config.map_loc(fragment.item.fg_type_condition.span);

    str_items :=
      List.append(
        str_items^,
        generate_text_struct(
          "graphql",
          loc,
          "```\n" ++ printed_type ++ "\n```",
        ),
      );
  };
};

let for_module_information = (loc: Location.t, module_name: option(string)) => {
  let module_name = module_name |> Option.get_or_else("GraphQL");
  str_module_information :=
    generate_text_struct(
      "graphql_module",
      loc |> conv_loc_from_ast,
      Printf.sprintf(
        {|**--- GraphQL PPX Module ---**

The contents of this module are automatically generated by `graphql-ppx`.
The following is simply an overview of the most important variables and types that you can access from this module.

```
module %s {
  // This is the stringified representation of your query, which gets sent to the server.
  let query: string;

  // This is the main type of the result you will get back.
  // You can hover above the identifier key (e.g. query or mutation) to see the fully generated type for your module.
  type t;

  // This function turns your raw result from the server into the reason/ocaml representation of that result.
  // Depending on your graphql client library, this process should happen automatically for you.
  let parse: Raw.t => t;

  // This function will prepare your data for sending it back to the server.
  // Depending on your graphql client library, this process should happen automatically for you.
  let serialize: t => Raw.t;

  // The definition tuple is primarily used to interact with client libraries.
  // The types are equivalent to: (parse, query, serialize).
  // Your client library will use these values to provide the properly parsed / serialized data for you.
  let definition: (
    Raw.t => t,
    string,
    t => Raw.t
  );

  // This is the representation of your raw result coming from the server.
  // It should not be necessary to access the types inside for normal use cases.
  module Raw: { type t; };
}
```|},
        module_name,
      ),
    );
};

let get_module = () => {
  switch (str_items^) {
  | [] => []
  | items => [
      Str.module_(
        Mb.mk(
          Location.mknoloc("Z__INTERNAL"),
          Mod.mk(
            Pmod_structure(List.append(items, str_module_information^)),
          ),
        ),
      ),
    ]
  };
};

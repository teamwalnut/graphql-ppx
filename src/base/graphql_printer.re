open Graphql_ast;
open Source_pos;

open Schema;

/*
   We rely on graphql_ppx.ml having created the result decoder before running
   the functions in this file. The result decoder produces proper and located
   error messages for missing or invalid types, which means that we can - for
   simplicity's sake - skip any kind of error handling here.

   The tricky part is of course to make sure that they rely on the same kind
   of validation logic :/
 */

type t =
  | Empty
  | String(string)
  | FragmentNameRef(string)
  | FragmentQueryRef(string);

let rec type_ref_name =
  fun
  | Named(n) => n
  | NonNull(t) => type_ref_name(t)
  | List(t) => type_ref_name(t);

let rec print_input_value = iv =>
  switch (iv) {
  | Iv_null => "null"
  | Iv_int(i) => string_of_int(i)
  | Iv_float(f) => string_of_float(f)
  | Iv_string(s) => "\"" ++ String.escaped(s) ++ "\""
  | Iv_boolean(b) => string_of_bool(b)
  | Iv_enum(s) => s
  | Iv_variable(v) => "$" ++ v
  | Iv_list(list) =>
    "["
    ++ (
      list
      |> List.map(({item, _}) => print_input_value(item))
      |> String.concat(", ")
    )
    ++ "]"
  | Iv_object(obj) =>
    "{"
    ++ (
      obj
      |> List.map((({item: key, _}, {item: value, _})) =>
           key ++ ": " ++ print_input_value(value)
         )
      |> String.concat(", ")
    )
    ++ "}"
  };

let print_argument = (({item: key, _}, {item: value, _})) =>
  key ++ ": " ++ print_input_value(value);

let print_arguments = args =>
  switch (args) {
  | [] => ""
  | args =>
    "(" ++ (List.map(print_argument, args) |> String.concat(", ")) ++ ")"
  };

let print_directive = d =>
  "@"
  ++ d.d_name.item
  ++ (
    switch (d.d_arguments) {
    | Some({item, _}) => print_arguments(item)
    | None => ""
    }
  );

let is_internal_directive = d =>
  switch (d.item.d_name.item) {
  | "bsVariant"
  | "bsRecord"
  | "bsObject"
  | "decoder"
  | "argumentDefinitions"
  | "arguments"
  | "bsField" => true
  | _ => false
  };

let print_directives = ds =>
  " "
  ++ (
    ds
    |> List.filter(d => (!) @@ is_internal_directive(d))
    |> List.map(d => print_directive(d.item))
    |> String.concat(" ")
  )
  ++ " ";

let print_fragment_spread = s => [|
  String("..."),
  FragmentNameRef(s.fs_name.item),
  String(" " ++ print_directives(s.fs_directives)),
|];

let rec print_type = ty =>
  switch (ty) {
  | Tr_named(n) => n.item
  | Tr_list(l) => "[" ++ print_type(l.item) ++ "]"
  | Tr_non_null_list(l) => "[" ++ print_type(l.item) ++ "]!"
  | Tr_non_null_named(n) => n.item ++ "!"
  };

let rec print_selection_set = (schema, ty, selection_set) =>
  switch (selection_set) {
  | [] => [||]
  | selection =>
    let add_typename =
      switch (ty) {
      | Interface(_)
      | Union(_) => true
      | Object({om_name, _}) =>
        let is_top_level_subscrption_type =
          schema.meta.sm_subscription_type == Some(om_name);

        !is_top_level_subscrption_type && Ppx_config.apollo_mode();
      | _ => false
      };

    let maybe_typename =
      if (add_typename) {
        String("__typename\n");
      } else {
        Empty;
      };

    let selection =
      selection
      |> List.map(s =>
           Array.append(print_selection(schema, ty, s), [|String("\n")|])
         )
      |> Array.concat;

    Array.concat([
      [|String("{\n"), maybe_typename|],
      selection,
      [|String("}\n")|],
    ]);
  }
and print_selection = (schema, ty, selection) =>
  switch (selection) {
  | Field({item, _}) => print_field(schema, ty, item)
  | FragmentSpread({item, _}) => print_fragment_spread(item)
  | InlineFragment({item, _}) => print_inline_fragment(schema, ty, item)
  }
and print_field = (schema, ty, f) => {
  let ty_fields =
    Option.unsafe_unwrap @@
    (
      switch (ty) {
      | Interface({im_fields, _}) => Some(im_fields)
      | Object({om_fields, _}) => Some(om_fields)
      | _ => None
      }
    );

  let field_ty =
    List.find(fm => fm.fm_name == f.fd_name.item, ty_fields).fm_field_type
    |> type_ref_name
    |> lookup_type(schema)
    |> Option.unsafe_unwrap;
  Array.append(
    [|
      switch (f.fd_alias) {
      | Some({item, _}) => String(item ++ ": ")
      | None => Empty
      },
      String(f.fd_name.item),
      switch (f.fd_arguments) {
      | Some({item, _}) => String(print_arguments(item))
      | None => Empty
      },
      String(print_directives(f.fd_directives)),
    |],
    switch (f.fd_selection_set) {
    | Some({item, _}) => print_selection_set(schema, field_ty, item)
    | None => [||]
    },
  );
}
and print_inline_fragment = (schema, ty, f) => {
  let inner_ty =
    switch (f.if_type_condition) {
    | Some({item, _}) => lookup_type(schema, item) |> Option.unsafe_unwrap
    | None => ty
    };
  Array.append(
    [|
      String("..."),
      String(
        switch (f.if_type_condition) {
        | Some({item, _}) => "on " ++ item ++ " "
        | None => " "
        },
      ),
      String(print_directives(f.if_directives)),
    |],
    print_selection_set(schema, inner_ty, f.if_selection_set.item),
  );
};

let print_variable_definition = ((name, def)) =>
  Printf.sprintf(
    "$%s: %s%s",
    name.item,
    print_type(def.vd_type.item),
    switch (def.vd_default_value) {
    | Some({item, _}) => " = " ++ print_input_value(item)
    | None => ""
    },
  );

let print_variable_definitions = defs =>
  "("
  ++ (List.map(print_variable_definition, defs) |> String.concat(", "))
  ++ ")";

let print_operation = (schema, op) => {
  let ty_name =
    switch (op.o_type) {
    | Query => schema.meta.sm_query_type
    | Mutation => Option.unsafe_unwrap(schema.meta.sm_mutation_type)
    | Subscription => Option.unsafe_unwrap(schema.meta.sm_subscription_type)
    };
  Array.append(
    [|
      String(
        switch (op.o_type) {
        | Query => "query "
        | Mutation => "mutation "
        | Subscription => "subscription "
        },
      ),
      switch (op.o_name) {
      | Some({item, _}) => String(item)
      | None => Empty
      },
      switch (op.o_variable_definitions) {
      | Some({item, _}) => String(print_variable_definitions(item))
      | None => Empty
      },
      String(print_directives(op.o_directives)),
    |],
    print_selection_set(
      schema,
      lookup_type(schema, ty_name) |> Option.unsafe_unwrap,
      op.o_selection_set.item,
    ),
  );
};

let print_fragment = (schema, f) =>
  Array.append(
    [|
      String(
        "fragment "
        ++ f.fg_name.item
        ++ " on "
        ++ f.fg_type_condition.item
        ++ " ",
      ),
      String(print_directives(f.fg_directives)),
    |],
    print_selection_set(
      schema,
      lookup_type(schema, f.fg_type_condition.item) |> Option.unsafe_unwrap,
      f.fg_selection_set.item,
    ),
  );

let print_definition = (schema, def) =>
  switch (def) {
  | Operation({item: operation, _}) => print_operation(schema, operation)
  | Fragment({item: fragment, _}) => print_fragment(schema, fragment)
  };

module StringSet = Set.Make(String);

let find_fragment_refs = parts =>
  parts
  |> Array.fold_left(
       acc =>
         fun
         | Empty => acc
         | String(_) => acc
         | FragmentNameRef(r) => StringSet.add(r, acc)
         | FragmentQueryRef(_) => acc,
       StringSet.empty,
     )
  |> StringSet.elements;

let compress_parts = (parts: array(t)) => {
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
           | (acc, Empty) => acc
           | (acc, curr) => [curr, ...acc]
           }
         },
         [],
       )
    |> List.rev
    |> Array.of_list
  );
};

let print_document = (schema, defs) => {
  let parts = defs |> List.map(print_definition(schema)) |> Array.concat;
  let fragment_refs = find_fragment_refs(parts);
  Array.concat([
    parts,
    fragment_refs |> Array.of_list |> Array.map(ref => FragmentQueryRef(ref)),
  ])
  |> compress_parts;
};

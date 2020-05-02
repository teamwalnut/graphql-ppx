open Result_structure;
open Graphql_ppx_base__;
open Generator_utils;
open Schema;
open Source_pos;
type path = list(string);

// extract the typeref
type extracted_type =
  | Type(Schema.type_meta)
  | TypeNotFound(string)
  | Nullable(extracted_type)
  | List(extracted_type);

type object_field =
  | Field({
      type_: Result_structure.t,
      loc_key: Source_pos.ast_location,
      loc: Source_pos.ast_location,
      path,
    })
  | Fragment({
      module_name: string,
      key: string,
      type_name: option(string),
    })
and type_def =
  | Object({
      loc: Source_pos.ast_location,
      variant_parent: bool,
      force_record: bool,
      path,
      fields: list(object_field),
    })
  | VariantSelection({
      loc: Source_pos.ast_location,
      path,
      fields: list((string, Result_structure.t)),
    })
  | VariantUnion({
      loc: Source_pos.ast_location,
      path,
      fields: list((Result_structure.name, Result_structure.t)),
    })
  | VariantInterface({
      loc: Source_pos.ast_location,
      path,
      base: (string, Result_structure.t),
      fields: list((string, Result_structure.t)),
    })
  | Enum({
      loc: Source_pos.ast_location,
      path,
      fields: list(string),
    });

type input_object_field =
  | InputField({
      type_: extracted_type,
      name: string,
      loc: Source_pos.ast_location,
      loc_type: option(Source_pos.ast_location),
    });

type arg_type_def =
  | InputObject({
      name: option(string),
      fields: list(input_object_field),
      loc: Source_pos.ast_location,
      is_recursive: bool,
    });

let generate_type_name = (~prefix="t") =>
  fun
  | [] => prefix
  | path => {
      path
      |> List.rev
      |> List.fold_left((acc, item) => acc ++ "_" ++ item, prefix);
    };

// function that generate types. It will output a nested list type descriptions
// later this result can be flattened and converted to an ast of combined type
// definitions
let rec extract = (~variant=false, ~path, ~raw) =>
  fun
  | Res_nullable(_loc, inner) => extract(~path, ~raw, inner)
  | Res_array(_loc, inner) => extract(~path, ~raw, inner)
  | Res_object(loc, _name, fields, type_name) as result_structure
  | Res_record(loc, _name, fields, type_name) as result_structure =>
    switch (result_structure, type_name, raw) {
    | (_, Some(type_name), false) => create_children(path, raw, fields)
    | (Res_record(_, _, _, _), _, false) =>
      create_object(path, raw, fields, true, loc, variant)
    | (_, _, _) => create_object(path, raw, fields, false, loc, variant)
    }
  | Res_poly_variant_union(loc, _name, fragments, _) => [
      VariantUnion({path, fields: fragments, loc}),
      ...extract_fragments(
           fragments
           |> List.map((({item: name}: Result_structure.name, t)) =>
                (name, t)
              ),
           path,
           raw,
         ),
    ]
  | Res_poly_variant_selection_set(loc, _name, fragments) => [
      VariantSelection({path, fields: fragments, loc}),
      ...extract_fragments(fragments, path, raw),
    ]
  | Res_poly_variant_interface(loc, _name, base, fragments) => [
      VariantInterface({path, fields: fragments, base, loc}),
      ...extract_fragments(fragments, path, raw),
    ]
  | Res_custom_decoder(_loc, _ident, inner) => extract(~path, ~raw, inner)
  | Res_solo_fragment_spread(_loc, _name, _) => []
  | Res_error(_loc, _message) => []
  | Res_id(_loc) => []
  | Res_string(_loc) => []
  | Res_int(_loc) => []
  | Res_float(_loc) => []
  | Res_boolean(_loc) => []
  | Res_raw_scalar(_) => []
  | Res_poly_enum(loc, enum_meta) => [
      Enum({
        path,
        fields: enum_meta.em_values |> List.map(({evm_name, _}) => evm_name),
        loc,
      }),
    ]
and fragment_names = f => f |> List.map(((name, _)) => name)
and extract_fragments = (fragments, path, raw) => {
  fragments
  |> List.fold_left(
       (acc, (name, inner)) =>
         List.append(
           extract(~variant=true, ~path=[name, ...path], ~raw, inner),
           acc,
         ),
       [],
     );
}
and create_children = (path, raw, fields) => {
  fields
  |> List.fold_left(
       acc =>
         fun
         | Fr_named_field({name, type_}) =>
           List.append(extract(~path=[name, ...path], ~raw, type_), acc)
         | Fr_fragment_spread(_key, _loc, _name, _, _arguments) => acc,
       [],
     );
}
and create_object = (path, raw, fields, force_record, loc, variant_parent) => {
  [
    Object({
      variant_parent,
      loc,
      force_record,
      path,
      fields:
        fields
        |> List.map(
             fun
             | Fr_named_field({name, loc, loc_key, type_}) => {
                 Field({loc, loc_key, path: [name, ...path], type_});
               }
             | Fr_fragment_spread(key, _loc, name, type_name, _arguments) =>
               Fragment({module_name: name, key, type_name}),
           ),
    }),
    ...create_children(path, raw, fields),
  ];
};

let raise_inconsistent_schema = (type_name, loc) =>
  raise_error_with_loc(
    loc,
    "Inconsistent schema, type named " ++ type_name ++ " cannot be found",
  );

let fetch_type = (schema, type_ref) => {
  let type_name = innermost_name(type_ref);
  (type_name, lookup_type(schema, type_name));
};

let rec convert_type_ref = schema =>
  fun
  | Named(type_name) =>
    Nullable(
      switch (lookup_type(schema, type_name)) {
      | None => TypeNotFound(type_name)
      | Some(type_) => Type(type_)
      },
    )
  | NonNull(Named(type_name)) =>
    switch (lookup_type(schema, type_name)) {
    | None => TypeNotFound(type_name)
    | Some(type_) => Type(type_)
    }
  | List(type_ref) => Nullable(List(convert_type_ref(schema, type_ref)))
  | NonNull(List(type_ref)) => List(convert_type_ref(schema, type_ref))
  // fold multiple non_nulls
  | NonNull(NonNull(inner)) => convert_type_ref(schema, NonNull(inner));

let generate_input_field_types =
    (
      _input_obj_name,
      schema: Schema.t,
      fields:
        list(
          (
            string,
            Schema.type_ref,
            Source_pos.ast_location,
            option(Source_pos.ast_location),
          ),
        ),
    ) => {
  fields
  |> List.fold_left(
       (acc, (name, type_ref, loc, loc_type)) => {
         [
           InputField({
             name,
             type_: convert_type_ref(schema, type_ref),
             loc,
             loc_type,
           }),
           ...acc,
         ]
       },
       [],
     )
  |> List.rev;
};

let rec get_inner_type = (type_: extracted_type) => {
  switch (type_) {
  | Type(_) => Some(type_)
  | Nullable(inner) => get_inner_type(inner)
  | List(inner) => get_inner_type(inner)
  | TypeNotFound(_) => None
  };
};

let get_input_object_name = (InputField({type_, _})) => {
  let type_ = get_inner_type(type_);
  switch (type_) {
  | Some(Type(InputObject({iom_name, _}))) => Some(iom_name)
  | _ => None
  };
};

let get_input_object_names = (fields: list(input_object_field)) => {
  fields
  |> List.map(get_input_object_name)
  |> List.fold_left(
       acc =>
         fun
         | Some(name) => [name, ...acc]
         | _ => acc,
       [],
     );
};

let rec extract_input_object =
        (
          schema: Schema.t,
          finalized_input_objects,
          (
            name: option(string),
            fields: list((string, Schema.type_ref, loc, option(loc))),
            loc,
          ),
        ) => {
  let gen_fields = generate_input_field_types(name, schema, fields);

  let is_recursive =
    switch (name) {
    | None => false
    | Some(name) =>
      gen_fields |> get_input_object_names |> List.exists(f => f == name)
    };

  [
    InputObject({name, fields: gen_fields, loc, is_recursive}),
    ...fields
       |> List.fold_left(
            (acc, (_name, type_ref, loc, _)) => {
              let (_type_name, type_) = fetch_type(schema, type_ref);
              switch (type_) {
              | Some(Schema.InputObject({iom_name, iom_input_fields, _})) =>
                let already_created_earlier =
                  finalized_input_objects |> List.exists(f => f == iom_name);
                let already_created_in_same_list =
                  acc
                  |> List.exists(
                       fun
                       | InputObject({name}) => name == Some(iom_name),
                     );

                if (already_created_earlier || already_created_in_same_list) {
                  // we already generated this input object
                  acc;
                } else {
                  let fields =
                    iom_input_fields
                    |> List.map(field =>
                         (field.am_name, field.am_arg_type, loc, None)
                       );

                  let result =
                    extract_input_object(
                      schema,
                      [iom_name, ...finalized_input_objects],
                      (Some(iom_name), fields, loc),
                    );

                  List.append(acc, result);
                };
              | _ => acc
              };
            },
            [],
          ),
  ];
};

let extract_args = (config, args): list(arg_type_def) =>
  switch (args) {
  | Some({item, span}) =>
    (
      None,
      item
      |> List.map(
           (({item: name, span}, {Graphql_ast.vd_type: variable_type, _})) =>
           (
             name,
             Type_utils.to_schema_type_ref(variable_type.item),
             config.map_loc(span),
             Some(config.map_loc(variable_type.span)),
           )
         ),
      config.map_loc(span),
    )
    |> extract_input_object(config.schema, [])
  | _ => []
  };

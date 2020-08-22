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
      arguments: Graphql_ast.arguments,
    })
  | Fragment({
      module_name: string,
      key: string,
      loc_key: Source_pos.ast_location,
      type_name: option(string),
    })
and type_def =
  | Object({
      loc: Source_pos.ast_location,
      variant_parent: bool,
      force_record: bool,
      path,
      existing_type: option(string),
      fields: list(object_field),
      interface_fragments:
        option((string, list((string, Result_structure.t)))),
    })
  | VariantSelection({
      loc: Source_pos.ast_location,
      path,
      fields: list((Result_structure.name, Result_structure.t)),
    })
  | VariantUnion({
      loc: Source_pos.ast_location,
      path,
      fields: list((Result_structure.name, Result_structure.t)),
      omit_future_value: bool,
    })
  | VariantInterface({
      name: string,
      loc: Source_pos.ast_location,
      path,
      fragments: list((string, Result_structure.t)),
    })
  | Enum({
      loc: Source_pos.ast_location,
      path,
      fields: list(string),
      omit_future_value: bool,
    });

type input_object_field =
  | InputField({
      type_: extracted_type,
      name: string,
      loc: Source_pos.ast_location,
      loc_type: option(Source_pos.ast_location),
    });

type arg_type_def =
  | NoVariables
  | InputObject({
      name: option(string),
      fields: list(input_object_field),
      loc: Source_pos.ast_location,
      is_recursive: bool,
    });

let has_required_variables = (arg_types: list(arg_type_def)) => {
  List.fold_left(
    has_required =>
      fun
      | NoVariables => has_required
      | InputObject({name: Some(_)}) => has_required
      | InputObject({name: None, fields}) =>
        List.fold_left(
          has_required =>
            fun
            | InputField({type_: Nullable(_)}) => has_required
            | InputField({type_: _}) => true,
          has_required,
          fields,
        ),
    false,
    arg_types,
  );
};

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
let rec extract = (~fragment_def=false, ~variant=false, ~path, ~raw) =>
  fun
  | Res_nullable({inner}) => extract(~path, ~raw, inner)
  | Res_array({inner}) => extract(~path, ~raw, inner)
  | Res_object({loc, fields, type_name, interface_fragments}) as result_structure
  | Res_record({loc, fields, type_name, interface_fragments}) as result_structure =>
    switch (result_structure, type_name, raw, fragment_def) {
    | (_, Some(_type_name), false, false) =>
      create_children(path, raw, fields)
    | (_, Some(_type_name), false, true) =>
      create_object(
        ~path,
        ~raw,
        ~force_record=true,
        ~loc,
        ~variant_parent=variant,
        ~existing_type=type_name,
        ~interface_fragments,
        fields,
      )
    | (Res_record(_), _, false, _) =>
      create_object(
        ~path,
        ~raw,
        ~force_record=true,
        ~loc,
        ~variant_parent=variant,
        ~existing_type=None,
        ~interface_fragments,
        fields,
      )
    | (_, _, _, _) =>
      create_object(
        ~path,
        ~raw,
        ~force_record=false,
        ~loc,
        ~variant_parent=variant,
        ~existing_type=None,
        ~interface_fragments,
        fields,
      )
    }
  | Res_poly_variant_union({loc, fragments, omit_future_value}) => [
      VariantUnion({path, fields: fragments, loc, omit_future_value}),
      ...extract_fragments(
           fragments
           |> List.map((({item: name}: Result_structure.name, t)) =>
                (name, t)
              ),
           path,
           raw,
         ),
    ]
  | Res_poly_variant_selection_set({loc, fragments}) => [
      VariantSelection({path, fields: fragments, loc}),
      ...extract_fragments(
           fragments
           |> List.map((({item: name}: Result_structure.name, t)) =>
                (name, t)
              ),
           path,
           raw,
         ),
    ]
  | Res_poly_variant_interface({loc, name, fragments}) => [
      VariantInterface({name, path, fragments, loc}),
      ...extract_fragments(fragments, [name, ...path], raw),
    ]
  | Res_custom_decoder({inner}) => extract(~path, ~raw, inner)
  | Res_solo_fragment_spread(_) => []
  | Res_error(_) => []
  | Res_id(_) => []
  | Res_string(_) => []
  | Res_int(_) => []
  | Res_float(_) => []
  | Res_boolean(_) => []
  | Res_raw_scalar(_) => []
  | Res_poly_enum({loc, enum_meta, omit_future_value}) => [
      Enum({
        path,
        fields: enum_meta.em_values |> List.map(({evm_name, _}) => evm_name),
        loc,
        omit_future_value,
      }),
    ]
and _fragment_names = f => f |> List.map(((name, _)) => name)
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
         | Fr_fragment_spread(_) => acc,
       [],
     );
}
and create_object =
    (
      ~path,
      ~raw,
      ~force_record,
      ~loc,
      ~variant_parent,
      ~existing_type,
      ~interface_fragments,
      fields,
    ) => {
  let object_fields =
    fields
    |> List.map(
         fun
         | Fr_named_field({name, loc, loc_key, type_, arguments}) => {
             Field({loc, loc_key, path: [name, ...path], type_, arguments});
           }
         | Fr_fragment_spread({key, loc: loc_key, name, type_name}) =>
           Fragment({module_name: name, key, loc_key, type_name}),
       );

  [
    [
      Object({
        variant_parent,
        loc,
        force_record,
        path,
        existing_type,
        fields: object_fields,
        interface_fragments,
      }),
    ],
    create_children(path, raw, fields),
    switch (interface_fragments) {
    | Some((_, [])) => []
    | Some((name, fragments)) => [
        VariantInterface({name, loc, path, fragments}),
        ...extract_fragments(fragments, [name, ...path], raw),
      ]
    | None => []
    },
  ]
  |> List.concat;
};

let _raise_inconsistent_schema = (type_name, loc) =>
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
                let finalized_input_objects_in_same_list =
                  acc
                  |> List.fold_left(
                       acc =>
                         fun
                         | InputObject({name: Some(name)}) => [name, ...acc]
                         | _ => acc,
                       [],
                     );

                let finalized_input_objects =
                  List.append(
                    finalized_input_objects,
                    finalized_input_objects_in_same_list,
                  );
                let already_created_earlier =
                  finalized_input_objects |> List.exists(f => f == iom_name);

                if (already_created_earlier) {
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
  | _ => [NoVariables]
  };

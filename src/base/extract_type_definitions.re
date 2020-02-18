open Result_structure;
open Graphql_ppx_base__;
open Generator_utils;
open Schema;
open Source_pos;

// extract the typeref
type extracted_type =
  | Type(Schema.type_meta)
  | TypeNotFound(string)
  | Nullable(extracted_type)
  | List(extracted_type);

type object_field =
  | Field({
      type_: Result_structure.t,
      loc: Source_pos.ast_location,
      path: list(string),
    })
  | Fragment({
      module_name: string,
      key: string,
      type_name: option(string),
    });

type type_def =
  | Object({
      force_record: bool,
      path: list(string),
      fields: list(object_field),
    });

type input_object_field =
  | InputField({
      type_: extracted_type,
      name: string,
      loc: Source_pos.ast_location,
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
let rec extract = path =>
  fun
  | Res_nullable(_loc, inner) => extract(path, inner)
  | Res_array(_loc, inner) => extract(path, inner)
  | Res_object(_loc, _obj_name, fields) => create_object(path, fields, false)
  | Res_record(_loc, _obj_name, fields) => create_object(path, fields, true)
  | Res_poly_variant_union(_loc, _name, fragments, _)
  | Res_poly_variant_selection_set(_loc, _name, fragments)
  | Res_poly_variant_interface(_loc, _name, _, fragments) =>
    fragments
    |> List.fold_left(
         (acc, (name, inner)) =>
           List.append(extract([name, ...path], inner), acc),
         [],
       )
  | Res_custom_decoder(_loc, _ident, inner) => extract(path, inner)
  | Res_solo_fragment_spread(_loc, _name, _) => []
  | Res_error(_loc, _message) => []
  | Res_id(_loc) => []
  | Res_string(_loc) => []
  | Res_int(_loc) => []
  | Res_float(_loc) => []
  | Res_boolean(_loc) => []
  | Res_raw_scalar(_) => []
  | Res_poly_enum(_loc, _enum_meta) => []
and create_object = (path, fields, force_record) => {
  [
    Object({
      force_record,
      path,
      fields:
        fields
        |> List.map(
             fun
             | Fr_named_field(name, loc, type_) =>
               Field({loc, path: [name, ...path], type_})
             | Fr_fragment_spread(key, _loc, name, type_name, _arguments) =>
               Fragment({module_name: name, key, type_name}),
           ),
    }),
    ...fields
       |> List.fold_left(
            acc =>
              fun
              | Fr_named_field(name, _loc, type_) =>
                List.append(extract([name, ...path], type_), acc)
              | Fr_fragment_spread(_key, _loc, _name, _, _arguments) => acc,
            [],
          ),
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
      schema: Schema.schema,
      fields: list((string, Schema.type_ref, Source_pos.ast_location)),
    ) => {
  fields
  |> List.fold_left(
       (acc, (name, type_ref, loc)) => {
         [
           InputField({name, type_: convert_type_ref(schema, type_ref), loc}),
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
          schema: Schema.schema,
          finalized_input_objects,
          (
            name: option(string),
            fields: list((string, Schema.type_ref, loc)),
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
            (acc, (_name, type_ref, loc)) => {
              let (_type_name, type_) = fetch_type(schema, type_ref);
              switch (type_) {
              | Some(InputObject({iom_name, iom_input_fields, _})) =>
                if (List.exists(f => f == iom_name, finalized_input_objects)) {
                  // we already generated this input object
                  acc;
                } else {
                  let fields =
                    iom_input_fields
                    |> List.map(field =>
                         (field.am_name, field.am_arg_type, loc)
                       );

                  let result =
                    extract_input_object(
                      schema,
                      [iom_name, ...finalized_input_objects],
                      (Some(iom_name), fields, loc),
                    );

                  List.append(acc, result);
                }
              | _ => acc
              };
            },
            [],
          ),
  ];
};

let extract_args =
    (
      config: output_config,
      args:
        option(
          spanning(
            list((spanning(string), Graphql_ast.variable_definition)),
          ),
        ),
    )
    : list(arg_type_def) =>
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
           )
         ),
      config.map_loc(span),
    )
    |> extract_input_object(config.schema, [])
  | _ => []
  };

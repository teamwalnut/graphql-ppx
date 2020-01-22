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
    });

// function that generate types. It will output a nested list type descriptions
// later this result can be flattened and converted to an ast of combined type
// definitions
let rec extract = path =>
  fun
  | Res_nullable(loc, inner) => extract(path, inner)
  | Res_array(loc, inner) => extract(path, inner)
  | Res_object(loc, obj_name, fields) => create_object(path, fields, false)
  | Res_record(loc, obj_name, fields) => create_object(path, fields, true)
  | Res_poly_variant_union(loc, name, fragments, _)
  | Res_poly_variant_interface(loc, name, _, fragments) =>
    fragments
    |> List.fold_left(
         (acc, (name, inner)) =>
           List.append(extract([name, ...path], inner), acc),
         [],
       )
  | Res_custom_decoder(loc, ident, inner) => extract(path, inner)
  | Res_solo_fragment_spread(loc, name) => []
  | Res_error(loc, message) => []
  | Res_id(loc) => []
  | Res_string(loc) => []
  | Res_int(loc) => []
  | Res_float(loc) => []
  | Res_boolean(loc) => []
  | Res_raw_scalar(_) => []
  | Res_poly_enum(loc, enum_meta) => []
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
             | Fr_fragment_spread(key, _loc, name, type_name) =>
               Fragment({module_name: name, key, type_name}),
           ),
    }),
    ...fields
       |> List.fold_left(
            acc =>
              fun
              | Fr_named_field(name, _loc, type_) =>
                List.append(extract([name, ...path], type_), acc)
              | Fr_fragment_spread(_key, _loc, _name, _) => acc,
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
      input_obj_name,
      schema: Schema.schema,
      fields: list((string, Schema.type_ref, Source_pos.ast_location)),
    ) => {
  fields
  |> List.fold_left(
       acc =>
         fun
         | (name, type_ref, loc) => {
             [
               InputField({
                 name,
                 type_: convert_type_ref(schema, type_ref),
                 loc,
               }),
               ...acc,
             ];
           },
       [],
     )
  |> List.rev;
};

let rec extract_input_object =
        (schema: Schema.schema, finalized_input_objects) => {
  fun
  | (
      name: option(string),
      fields: list((string, Schema.type_ref, loc)),
      loc,
    ) => {
      [
        InputObject({
          name,
          fields: generate_input_field_types(name, schema, fields),
          loc,
        }),
        ...fields
           |> List.fold_left(
                acc =>
                  fun
                  | (name, type_ref, loc) => {
                      let (type_name, type_) = fetch_type(schema, type_ref);
                      switch (type_) {
                      | Some(InputObject({iom_name, iom_input_fields})) =>
                        if (List.exists(
                              f => f == iom_name,
                              finalized_input_objects,
                            )) {
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
};

let extract_args:
  (
    output_config,
    option(
      spanning(list((spanning(string), Graphql_ast.variable_definition))),
    )
  ) =>
  list(arg_type_def) =
  config => {
    fun
    | Some({item, span}) => {
        (
          None,
          item
          |> List.map(
               fun
               | (
                   {item: name, span},
                   {Graphql_ast.vd_type: variable_type, _},
                 ) => (
                   name,
                   Type_utils.to_schema_type_ref(variable_type.item),
                   config.map_loc(span),
                 ),
             ),
          config.map_loc(span),
        )
        |> extract_input_object(config.schema, []);
      }
    | _ => [];
  };

open Result_structure;
open Graphql_ppx_base__;
open Generator_utils;
open Schema;
open Source_pos;

type nullable =
  | NonNull
  | Null;
// extract the typeref
type extracted_type =
  | Type(nullable, option(Schema.type_meta))
  | List(nullable, extracted_type);

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

type input_object_field =
  | InputField({
      type_: extracted_type,
      path: list(string),
    });

type type_def =
  | Object({
      path: list(string),
      fields: list(object_field),
    })
  | InputObject({
      path: list(string),
      fields: list(input_object_field),
    });

// function that generate types. It will output a nested list type descriptions
// later this result can be flattened and converted to an ast of combined type
// definitions
let rec extract = path =>
  fun
  | Res_nullable(loc, inner) => extract(path, inner)
  | Res_array(loc, inner) => extract(path, inner)
  | Res_object(loc, obj_name, fields)
  | Res_record(loc, obj_name, fields) => {
      // let path = List.length(path) == 0 ? [obj_name] : path;
      let path = path;
      [
        Object({
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
    }
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
  | Res_poly_enum(loc, enum_meta) => [];

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
  | Named(type_name) => Type(Null, lookup_type(schema, type_name))
  | NonNull(Named(type_name)) =>
    Type(NonNull, lookup_type(schema, type_name))
  | List(type_ref) => List(Null, convert_type_ref(schema, type_ref))
  | NonNull(List(type_ref)) =>
    List(NonNull, convert_type_ref(schema, type_ref))
  // fold multiple non_nulls
  | NonNull(NonNull(inner)) => convert_type_ref(schema, NonNull(inner));

let generate_input_field_types =
    (path, schema: Schema.schema, fields: list((string, Schema.type_ref))) => {
  fields
  |> List.fold_left(
       acc =>
         fun
         | (name, type_ref) => {
             [
               InputField({
                 type_: convert_type_ref(schema, type_ref),
                 path: [name, ...path],
               }),
               ...acc,
             ];
           },
       [],
     )
  |> List.rev;
};

let rec extract_input_object =
        (path: list(string), schema: Graphql_ppx_base.Schema.schema) => {
  fun
  | (name: string, fields: list((string, Schema.type_ref))) => [
      InputObject({
        path,
        fields: generate_input_field_types(path, schema, fields),
      }),
      ...fields
         |> List.fold_left(
              acc =>
                fun
                | (name, type_ref) => {
                    let (type_name, type_) = fetch_type(schema, type_ref);
                    switch (type_) {
                    | Some(InputObject({iom_name, iom_input_fields})) =>
                      let fields =
                        iom_input_fields
                        |> List.map(field =>
                             (field.am_name, field.am_arg_type)
                           );

                      let result =
                        extract_input_object(
                          [name, ...path],
                          schema,
                          (iom_name, fields),
                        );

                      List.append(acc, result);
                    | _ => acc
                    };
                  },
              [],
            ),
    ];
};

let extract_args:
  (
    Graphql_ppx_base.Schema.schema,
    option(
      spanning(list((spanning(string), Graphql_ast.variable_definition))),
    )
  ) =>
  list(type_def) =
  schema => {
    fun
    | Some({item, _}) => {
        (
          "",
          item
          |> List.map(((span, {Graphql_ast.vd_type: variable_type, _})) =>
               (span.item, Type_utils.to_schema_type_ref(variable_type.item))
             ),
        )
        |> extract_input_object([], schema);
      }
    | _ => [];
  };

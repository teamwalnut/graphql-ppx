type path = list(string);

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
      loc: Source_pos.ast_location,
      path,
      base: (string, Result_structure.t),
      fields: list((string, Result_structure.t)),
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

let extract:
  (
    ~fragment_def: bool=?,
    ~variant: bool=?,
    ~path: path,
    ~raw: bool,
    Result_structure.t
  ) =>
  list(type_def);
let generate_type_name: (~prefix: string=?, path) => string;
let extract_args:
  (
    Generator_utils.output_config,
    option(Source_pos.spanning(Graphql_ast.variable_definitions))
  ) =>
  list(arg_type_def);

let get_inner_type: extracted_type => option(extracted_type);

let has_required_variables: list(arg_type_def) => bool;

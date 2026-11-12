type path = string list

type extracted_type =
  | Type of Schema.type_meta
  | TypeNotFound of string
  | Nullable of extracted_type
  | List of extracted_type

type object_field =
  | Field of {
      type_ : Result_structure.t;
      loc_key : Source_pos.ast_location;
      loc : Source_pos.ast_location;
      path : path;
      arguments : Graphql_ast.arguments;
    }
  | Fragment of {
      module_name : string;
      key : string;
      loc_key : Source_pos.ast_location;
      type_name : string option;
    }

and type_def =
  | Object of {
      loc : Source_pos.ast_location;
      variant_parent : bool;
      path : path;
      existing_type : string option;
      fields : object_field list;
      interface_fragments : (string * (string * Result_structure.t) list) option;
    }
  | VariantSelection of {
      loc : Source_pos.ast_location;
      path : path;
      fields : (Result_structure.name * Result_structure.t) list;
    }
  | VariantUnion of {
      loc : Source_pos.ast_location;
      path : path;
      fields : (Result_structure.name * Result_structure.t) list;
      omit_future_value : bool;
    }
  | VariantInterface of {
      name : string;
      loc : Source_pos.ast_location;
      path : path;
      fragments : (string * Result_structure.t) list;
    }
  | Enum of {
      loc : Source_pos.ast_location;
      path : path;
      fields : string list;
      omit_future_value : bool;
    }

type input_object_field =
  | InputField of {
      type_ : extracted_type;
      name : string;
      loc : Source_pos.ast_location;
      loc_type : Source_pos.ast_location option;
    }

type arg_type_def =
  | NoVariables
  | InputObject of {
      name : string option;
      fields : input_object_field list;
      loc : Source_pos.ast_location;
      is_recursive : bool;
    }

val extract :
  ?fragment_def:bool ->
  ?variant:bool ->
  path:path ->
  raw:bool ->
  Result_structure.t ->
  type_def list

val generate_type_name : ?prefix:string -> path -> string

val extract_args :
  Generator_utils.output_config ->
  Graphql_ast.variable_definitions Source_pos.spanning option ->
  arg_type_def list

val get_inner_type : extracted_type -> extracted_type option
val has_required_variables : arg_type_def list -> bool

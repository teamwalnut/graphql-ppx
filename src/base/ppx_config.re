type output_mode =
  | String
  | Apollo_AST;

type fragment_in_query =
  | Include
  | Exclude;

type config = {
  verbose_logging: bool,
  output_mode,
  verbose_error_handling: bool,
  apollo_mode: bool,
  root_directory: string,
  schema_file: string,
  raise_error_with_loc: 'a. (Source_pos.ast_location, string) => 'a,
  template_tag: option(string),
  template_tag_location: option(string),
  template_tag_import: option(string),
  template_tag_return_type: option(string),
  template_tag_is_function: option(bool),
  custom_fields: Hashtbl.t(string, string),
  future_added_value: bool,
  extend_query: option(string),
  extend_query_no_required_variables: option(string),
  extend_mutation: option(string),
  extend_mutation_no_required_variables: option(string),
  extend_subscription: option(string),
  extend_subscription_no_required_variables: option(string),
  extend_fragment: option(string),
  fragment_in_query,
  native: bool,
};

let config_ref = ref(None);

let set_config = config => config_ref := Some(config);

let update_config = update => config_ref := config_ref^ |> Option.map(update);

let unsafe_unwrap_config = Option.unsafe_unwrap(~reason="Cannot read config");

let verbose_logging = () =>
  (config_ref^ |> unsafe_unwrap_config).verbose_logging;

let output_mode = () => (config_ref^ |> unsafe_unwrap_config).output_mode;

let custom_fields = () => (config_ref^ |> unsafe_unwrap_config).custom_fields;

let future_added_value = () =>
  (config_ref^ |> unsafe_unwrap_config).future_added_value;

let apollo_mode = () => (config_ref^ |> unsafe_unwrap_config).apollo_mode;

let template_tag = () => (config_ref^ |> unsafe_unwrap_config).template_tag;
let fragment_in_query = () =>
  (config_ref^ |> unsafe_unwrap_config).fragment_in_query;

let template_tag_import = () =>
  (config_ref^ |> unsafe_unwrap_config).template_tag_import;
let template_tag_location = () =>
  (config_ref^ |> unsafe_unwrap_config).template_tag_location;
let template_tag_return_type = () =>
  (config_ref^ |> unsafe_unwrap_config).template_tag_return_type;

let template_tag_is_function = () =>
  (config_ref^ |> unsafe_unwrap_config).template_tag_is_function;

let verbose_error_handling = () =>
  (config_ref^ |> unsafe_unwrap_config).verbose_error_handling;

let root_directory = () =>
  (config_ref^ |> unsafe_unwrap_config).root_directory;

let schema_file = () => (config_ref^ |> unsafe_unwrap_config).schema_file;

let extend_query = () => (config_ref^ |> unsafe_unwrap_config).extend_query;
let extend_query_no_required_variables = () =>
  (config_ref^ |> unsafe_unwrap_config).extend_query_no_required_variables;
let extend_mutation = () =>
  (config_ref^ |> unsafe_unwrap_config).extend_mutation;
let extend_mutation_no_required_variables = () =>
  (config_ref^ |> unsafe_unwrap_config).extend_mutation_no_required_variables;

let extend_subscription = () =>
  (config_ref^ |> unsafe_unwrap_config).extend_subscription;
let extend_subscription_no_required_variables = () =>
  (config_ref^ |> unsafe_unwrap_config).
    extend_subscription_no_required_variables;
let extend_fragment = () =>
  (config_ref^ |> unsafe_unwrap_config).extend_fragment;

let native = () => (config_ref^ |> unsafe_unwrap_config).native;

let raise_error_with_loc = (loc, message) =>
  (config_ref^ |> unsafe_unwrap_config).raise_error_with_loc(loc, message);

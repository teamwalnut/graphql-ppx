type output_mode =
  | String
  | Apollo_AST;

type config = {
  verbose_logging: bool,
  output_mode,
  verbose_error_handling: bool,
  apollo_mode: bool,
  root_directory: string,
  schema_file: string,
  raise_error_with_loc: 'a. (Source_pos.ast_location, string) => 'a,
  records: bool,
  legacy: bool,
  template_tag: option(string),
  template_tag_location: option(string),
  template_tag_import: option(string),
  custom_fields: Hashtbl.t(string, string),
  future_added_value: bool,
  extend_query: option(string),
  extend_mutation: option(string),
  extend_subscription: option(string),
  extend_fragment: option(string),
};

let config_ref = ref(None);

let set_config = config => config_ref := Some(config);

let update_config = update => config_ref := config_ref^ |> Option.map(update);

let verbose_logging = () =>
  (config_ref^ |> Option.unsafe_unwrap).verbose_logging;

let output_mode = () => (config_ref^ |> Option.unsafe_unwrap).output_mode;

let custom_fields = () => (config_ref^ |> Option.unsafe_unwrap).custom_fields;

let future_added_value = () =>
  (config_ref^ |> Option.unsafe_unwrap).future_added_value;

let apollo_mode = () => (config_ref^ |> Option.unsafe_unwrap).apollo_mode;

let records = () => (config_ref^ |> Option.unsafe_unwrap).records;
let legacy = () => (config_ref^ |> Option.unsafe_unwrap).legacy;

let template_tag = () => (config_ref^ |> Option.unsafe_unwrap).template_tag;
let template_tag_import = () =>
  (config_ref^ |> Option.unsafe_unwrap).template_tag_import;
let template_tag_location = () =>
  (config_ref^ |> Option.unsafe_unwrap).template_tag_location;

let verbose_error_handling = () =>
  (config_ref^ |> Option.unsafe_unwrap).verbose_error_handling;

let root_directory = () =>
  (config_ref^ |> Option.unsafe_unwrap).root_directory;

let schema_file = () => (config_ref^ |> Option.unsafe_unwrap).schema_file;

let extend_query = () => (config_ref^ |> Option.unsafe_unwrap).extend_query;
let extend_mutation = () =>
  (config_ref^ |> Option.unsafe_unwrap).extend_mutation;
let extend_subscription = () =>
  (config_ref^ |> Option.unsafe_unwrap).extend_subscription;
let extend_fragment = () =>
  (config_ref^ |> Option.unsafe_unwrap).extend_fragment;

let raise_error_with_loc = (loc, message) =>
  (config_ref^ |> Option.unsafe_unwrap).raise_error_with_loc(loc, message);

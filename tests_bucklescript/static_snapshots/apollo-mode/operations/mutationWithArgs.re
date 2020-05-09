[@ocaml.ppx.context
  {
    tool_name: "migrate_driver",
    include_dirs: [],
    load_path: [],
    open_modules: [],
    for_package: None,
    debug: false,
    use_threads: false,
    use_vmthreads: false,
    recursive_types: false,
    principal: false,
    transparent_modules: false,
    unboxed_types: false,
    unsafe_string: false,
    cookies: [],
  }
];
module MyQuery = {
  module Raw = {
    type t = {optionalInputArgs: string};
    type t_variables = {required: string};
  };
  let query = "mutation MyMutation($required: String!)  {\noptionalInputArgs(required: $required, anotherRequired: \"val\")  \n}\n";
  type t = {optionalInputArgs: string};
  type operation = t;
  type t_variables = {required: string};
  let parse: Raw.t => t =
    (value) => (
      {
        optionalInputArgs: {
          let value = (value: Raw.t).optionalInputArgs;
          value;
        },
      }: t
    );
  let serialize: t => Raw.t =
    (value) => (
      {
        let optionalInputArgs = {
          let value = (value: t).optionalInputArgs;
          value;
        };
        {optionalInputArgs: optionalInputArgs};
      }: Raw.t
    );
  let serializeVariables: t_variables => Raw.t_variables =
    inp => {required: (a => a)((inp: t_variables).required)};
  let makeVariables = (~required, ()) =>
    serializeVariables({required: required}: t_variables);
  let definition = (parse, query, serialize);
};

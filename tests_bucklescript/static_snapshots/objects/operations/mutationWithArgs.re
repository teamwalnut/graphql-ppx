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
    type t = {. "optionalInputArgs": string};
    type t_variables = {. "required": string};
  };
  let query = "mutation MyMutation($required: String!)  {\noptionalInputArgs(required: $required, anotherRequired: \"val\")  \n}\n";
  type t = {. "optionalInputArgs": string};
  type t_variables = {. "required": string};
  let parse: Raw.t => t =
    value => {

      "optionalInputArgs": {
        let value = value##optionalInputArgs;

        value;
      },
    };
  let serialize: t => Raw.t =
    value => {
      let optionalInputArgs = {
        let value = value##optionalInputArgs;

        value;
      };
      {

        "optionalInputArgs": optionalInputArgs,
      };
    };
  let serializeVariables: t_variables => Js.Json.t =
    inp =>
      [|("required", (a => Some(Js.Json.string(a)))(inp##required))|]
      |> Js.Array.filter(
           fun
           | (_, None) => false
           | (_, Some(_)) => true,
         )
      |> Js.Array.map(
           fun
           | (k, Some(v)) => (k, v)
           | (k, None) => (k, Js.Json.null),
         )
      |> Js.Dict.fromArray
      |> Js.Json.object_;
  let makeVariables = (~required, ()) =>
    serializeVariables(
      {

        "required": required,
      }: t_variables,
    );
  let definition = (parse, query, serialize);
};

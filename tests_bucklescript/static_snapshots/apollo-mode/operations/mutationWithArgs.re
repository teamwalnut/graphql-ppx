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
    type t = {
      __typename: string,
      optionalInputArgs: string,
    };
  };
  let query = "mutation MyMutation($required: String!)  {\n__typename  \noptionalInputArgs(required: $required, anotherRequired: \"val\")  \n}\n";
  type t = {
    __typename: string,
    optionalInputArgs: string,
  };
  type t_variables = {required: string};
  let parse: Raw.t => t =
    (value) => (
      {

        __typename: {
          let value = (value: Raw.t).__typename;

          value;
        },

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
        }
        and __typename = {
          let value = (value: t).__typename;

          value;
        };
        {

          __typename,

          optionalInputArgs,
        };
      }: Raw.t
    );
  let serializeVariables: t_variables => Js.Json.t =
    inp =>
      [|("required", (a => Some(Js.Json.string(a)))(inp.required))|]
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
  let makeVar = (~f, ~required, ()) =>
    f(
      serializeVariables(
        {

          required: required,
        }: t_variables,
      ),
    );
  let definition = (parse, query, makeVar);
  let makeVariables = makeVar(~f=f => f);
};

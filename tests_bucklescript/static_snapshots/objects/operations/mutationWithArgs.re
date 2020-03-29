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
  };
  let query = "mutation MyMutation($required: String!)  {\noptionalInputArgs(required: $required, anotherRequired: \"val\")  \n}\n";
  type t = {. "optionalInputArgs": string};
  type t_variables = {. "required": string};
  let parse: Js.Json.t => t =
    value => {
      [@metaloc loc]
      let value = value |> Js.Json.decodeObject |> Js.Option.getExn;
      {

        "optionalInputArgs": {
          let value =
            Js.Dict.unsafeGet(Obj.magic(value), "optionalInputArgs");

          (Obj.magic(value): string);
        },
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
  let makeVar = (~f, ~required, ()) =>
    f(
      serializeVariables(
        {

          "required": required,
        }: t_variables,
      ),
    );
  let definition = (parse, query, makeVar);
  let makeVariables = makeVar(~f=f => f);
};

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
    type t = {argNamedQuery: int};
  };
  let query = "query ($query: String!)  {\nargNamedQuery(query: $query)  \n}\n";
  type t = {argNamedQuery: int};
  type t_variables = {query: string};
  let parse: Raw.t => t =
    (value) => (
      {

        argNamedQuery: {
          let value = (value: Raw.t).argNamedQuery;

          value;
        },
      }: t
    );
  let serialize: t => Raw.t =
    (value) => (
      {
        let argNamedQuery = {
          let value = (value: t).argNamedQuery;

          value;
        };
        {

          argNamedQuery: argNamedQuery,
        };
      }: Raw.t
    );
  let serializeVariables: t_variables => Js.Json.t =
    inp =>
      [|("query", (a => Some(Js.Json.string(a)))(inp.query))|]
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
  let makeVariables = (~query, ()) =>
    serializeVariables(
      {

        query: query,
      }: t_variables,
    );
  let definition = (parse, query, serialize);
};

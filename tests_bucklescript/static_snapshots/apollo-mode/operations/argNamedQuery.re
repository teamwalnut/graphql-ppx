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
    type t_variables = {query: int};
  };
  let query = "query ($query: Int!)  {\nargNamedQuery(query: $query)  \n}\n";
  type t = {argNamedQuery: int};
  type operation = t;
  type t_variables = {query: int};
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
        {argNamedQuery: argNamedQuery};
      }: Raw.t
    );
  let serializeVariables: t_variables => Raw.t_variables =
    inp => {query: (a => a)((inp: t_variables).query)};
  let makeVariables = (~query, ()) =>
    serializeVariables({query: query}: t_variables);
  let definition = (parse, query, serialize);
};

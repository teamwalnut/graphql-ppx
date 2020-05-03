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
    type t = {. "argNamedQuery": int};
    type t_variables = {. "query": string};
  };
  let query = "query ($query: String!)  {\nargNamedQuery(query: $query)  \n}\n";
  type t = {. "argNamedQuery": int};
  type operation = t;
  type t_variables = {. "query": string};
  let parse: Raw.t => t =
    value => {
      let argNamedQuery = {
        let value = value##argNamedQuery;
        value;
      };
      {"argNamedQuery": argNamedQuery};
    };
  let serialize: t => Raw.t =
    value => {
      let argNamedQuery = {
        let value = value##argNamedQuery;
        value;
      };
      {"argNamedQuery": argNamedQuery};
    };
  let serializeVariables: t_variables => Raw.t_variables =
    inp => {"query": (a => a)(inp##query)};
  let make = (~query, ()) => {
    "query": query,
    "variables": serializeVariables({"query": query}: t_variables),
    "parse": parse,
  }
  and makeVariables = (~query, ()) =>
    serializeVariables({"query": query}: t_variables);
  let makeWithVariables = variables => {
    "query": query,
    "variables": serializeVariables(variables),
    "parse": parse,
  };
  let definition = (parse, query, serialize);
};

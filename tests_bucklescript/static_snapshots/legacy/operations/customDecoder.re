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
module StringOfInt = {
  let parse = string_of_int;
  type t = string;
};
module IntOfString = {
  let parse = int_of_string;
  type t = int;
};

module MyQuery = {
  module Raw = {
    type t = {. "variousScalars": t_variousScalars}
    and t_variousScalars = {
      .
      "string": string,
      "int": int,
    };
  };
  let query = "query   {\nvariousScalars  {\nstring  \nint  \n}\n\n}\n";
  type t = {. "variousScalars": t_variousScalars}
  and t_variousScalars = {
    .
    "string": IntOfString.t,
    "int": StringOfInt.t,
  };
  let parse: Raw.t => t =
    value => {

      "variousScalars": {
        let value = value##variousScalars;
        {

          "string": {
            let value = value##string;

            IntOfString.parse(value);
          },

          "int": {
            let value = value##int;

            StringOfInt.parse(value);
          },
        };
      },
    };
  let serialize: t => Raw.t =
    value => {

      "variousScalars": {
        let value = value##variousScalars;
        {

          "string": {
            let value = value##string;

            IntOfString.serialize(value);
          },

          "int": {
            let value = value##int;

            StringOfInt.serialize(value);
          },
        };
      },
    };
  let makeVar = (~f, ()) => f(Js.Json.null);
  let make =
    makeVar(~f=variables =>
      {"query": query, "variables": variables, "parse": parse}
    );
  let makeWithVariables = variables => {
    "query": query,
    "variables": serializeVariables(variables),
    "parse": parse,
  };
  let definition = (parse, query, makeVar);
};

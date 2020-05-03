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
    type t = {. "defaultObjectValueOnScalar": string};
  };
  let query = "query   {\ndefaultObjectValueOnScalar(filter: {some: {json: \"value\"}}, arg: {field: \"otherValue\"})  \n}\n";
  type t = {. "defaultObjectValueOnScalar": string};
  let parse: Raw.t => t =
    value => {
      let defaultObjectValueOnScalar = {
        let value = value##defaultObjectValueOnScalar;
        value;
      };
      {

        "defaultObjectValueOnScalar": defaultObjectValueOnScalar,
      };
    };
  let serialize: t => Raw.t =
    value => {
      let defaultObjectValueOnScalar = {
        let value = value##defaultObjectValueOnScalar;

        value;
      };
      {

        "defaultObjectValueOnScalar": defaultObjectValueOnScalar,
      };
    };
  let make = () => {
    "query": query,
    "variables": Js.Json.null,
    "parse": parse,
  };
  let definition = (parse, query, serialize);
};

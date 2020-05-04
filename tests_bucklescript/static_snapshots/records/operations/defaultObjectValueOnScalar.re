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
    type t = {defaultObjectValueOnScalar: string};
  };
  let query = "query   {\ndefaultObjectValueOnScalar(filter: {some: {json: \"value\"}}, arg: {field: \"otherValue\"})  \n}\n";
  type t = {defaultObjectValueOnScalar: string};
  type operation = t;
  let parse: Raw.t => t =
    (value) => (
      {
        let defaultObjectValueOnScalar = {
          let value = (value: Raw.t).defaultObjectValueOnScalar;
          value;
        };
        {defaultObjectValueOnScalar: defaultObjectValueOnScalar};
      }: t
    );
  let serialize: t => Raw.t =
    (value) => (
      {
        let defaultObjectValueOnScalar = {
          let value = (value: t).defaultObjectValueOnScalar;
          value;
        };
        {defaultObjectValueOnScalar: defaultObjectValueOnScalar};
      }: Raw.t
    );
  let definition = (parse, query, serialize);
};

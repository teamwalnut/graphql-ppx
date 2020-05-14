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
  let serialize = int_of_string;
  type t = string;
};
module IntOfString = {
  let parse = int_of_string;
  let serialize = string_of_int;
  type t = int;
};
module MyQuery = {
  module Raw = {
    type t_variousScalars = {
      string,
      int,
    };
    type t = {variousScalars: t_variousScalars};
  };
  let query = "query   {\nvariousScalars  {\nstring  \nint  \n}\n\n}\n";
  type t_variousScalars = {
    string: IntOfString.t,
    int: StringOfInt.t,
  };
  type t = {variousScalars: t_variousScalars};
  let parse: Raw.t => t =
    (value) => (
      {
        let variousScalars = {
          let value = (value: Raw.t).variousScalars;
          (
            {
              let int = {
                let value = (value: Raw.t_variousScalars).int;
                StringOfInt.parse(value);
              }
              and string = {
                let value = (value: Raw.t_variousScalars).string;
                IntOfString.parse(value);
              };
              {

                string,

                int,
              };
            }: t_variousScalars
          );
        };
        {

          variousScalars: variousScalars,
        };
      }: t
    );
  let serialize: t => Raw.t =
    (value) => (
      {
        let variousScalars = {
          let value = (value: t).variousScalars;
          (
            {
              let int = {
                let value = (value: t_variousScalars).int;

                StringOfInt.serialize(value);
              }
              and string = {
                let value = (value: t_variousScalars).string;

                IntOfString.serialize(value);
              };
              {

                string,

                int,
              };
            }: Raw.t_variousScalars
          );
        };
        {

          variousScalars: variousScalars,
        };
      }: Raw.t
    );
  let definition = (parse, query, serialize);
};

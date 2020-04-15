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
    type t = {variousScalars: t_variousScalars}
    and t_variousScalars = {
      string,
      int,
    };
  };
  let query = "query   {\nvariousScalars  {\nstring  \nint  \n}\n\n}\n";
  type t = {variousScalars: t_variousScalars}
  and t_variousScalars = {
    string: IntOfString.t,
    int: StringOfInt.t,
  };
  let parse: Raw.t => t =
    (value) => (
      {

        variousScalars: {
          let value = (value: Raw.t).variousScalars;
          (
            {

              string: {
                let value = (value: Raw.t_variousScalars).string;

                IntOfString.parse(value);
              },

              int: {
                let value = (value: Raw.t_variousScalars).int;

                StringOfInt.parse(value);
              },
            }: t_variousScalars
          );
        },
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
  let makeVar = (~f, ()) => f(Js.Json.null);
  let definition = (parse, query, makeVar);
};

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
    type t_variousScalars = {
      __typename: string,
      string,
      int,
    };
    type t = {variousScalars: t_variousScalars};
  };
  let query = "query   {\nvariousScalars  {\n__typename  \nstring  \nint  \n}\n\n}\n";
  type t_variousScalars = {
    __typename: string,
    string: IntOfString.t,
    int: StringOfInt.t,
  };
  type t = {variousScalars: t_variousScalars};
  let parse: Raw.t => t =
    (value) => (
      {
        variousScalars: {
          let value = (value: Raw.t).variousScalars;
          (
            {
              __typename: {
                let value = (value: Raw.t_variousScalars).__typename;
                value;
              },
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
              }
              and __typename = {
                let value = (value: t_variousScalars).__typename;

                value;
              };
              {

                __typename,

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

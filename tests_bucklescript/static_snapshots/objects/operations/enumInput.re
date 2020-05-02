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
    type t = {. "enumInput": string};
    type t_variables = {. "arg": string};
  };
  let query = "query ($arg: SampleField!)  {\nenumInput(arg: $arg)  \n}\n";
  type t = {. "enumInput": string};
  type t_variables = {. "arg": [ | `FIRST | `SECOND | `THIRD]};
  let parse: Raw.t => t =
    value => {
      let enumInput = {
        let value = value##enumInput;
        value;
      };
      {

        "enumInput": enumInput,
      };
    };
  let serialize: t => Raw.t =
    value => {
      let enumInput = {
        let value = value##enumInput;

        value;
      };
      {

        "enumInput": enumInput,
      };
    };
  let serializeVariables: t_variables => Raw.t_variables =
    inp => {

      "arg":
        (
          a =>
            switch (a) {
            | `FIRST => "FIRST"
            | `SECOND => "SECOND"
            | `THIRD => "THIRD"
            }
        )(
          inp##arg,
        ),
    };
  let makeVariables = (~arg, ()) =>
    serializeVariables(
      {

        "arg": arg,
      }: t_variables,
    );
  let definition = (parse, query, serialize);
};

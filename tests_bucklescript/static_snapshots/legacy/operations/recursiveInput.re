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
    type t = {. "recursiveInput": string};
    type t_variables = {. "arg": t_variables_RecursiveInput}
    and t_variables_RecursiveInput = {
      .
      "otherField": Js.Nullable.t(string),
      "inner": Js.Nullable.t(t_variables_RecursiveInput),
      "enum": Js.Nullable.t(string),
    };
  };
  let query = "query ($arg: RecursiveInput!)  {\nrecursiveInput(arg: $arg)  \n}\n";
  type t = {. "recursiveInput": string};
  type t_variables = {. "arg": t_variables_RecursiveInput}
  and t_variables_RecursiveInput = {
    .
    "otherField": option(string),
    "inner": option(t_variables_RecursiveInput),
    "enum": option([ | `FIRST | `SECOND | `THIRD]),
  };
  let parse: Raw.t => t =
    value => {
      "recursiveInput": {
        let value = value##recursiveInput;
        value;
      },
    };
  let serialize: t => Raw.t =
    value => {
      let recursiveInput = {
        let value = value##recursiveInput;

        value;
      };
      {

        "recursiveInput": recursiveInput,
      };
    };

  let rec serializeVariables: t_variables => Raw.t_variables =
    inp => {

      arg: (a => serializeInputObjectRecursiveInput(a))(inp##arg),
    }

  and serializeInputObjectRecursiveInput:
    t_variables_RecursiveInput => Raw.t_variables_RecursiveInput =
    inp => {

      otherField:
        (
          a =>
            switch (a) {
            | None => Js.Nullable.undefined
            | Some(b) => Js.Nullable.return((a => a)(b))
            }
        )(
          inp##otherField,
        ),

      inner:
        (
          a =>
            switch (a) {
            | None => Js.Nullable.undefined
            | Some(b) =>
              Js.Nullable.return(
                (a => serializeInputObjectRecursiveInput(a))(b),
              )
            }
        )(
          inp##inner,
        ),

      enum:
        (
          a =>
            switch (a) {
            | None => Js.Nullable.undefined
            | Some(b) =>
              Js.Nullable.return(
                (
                  a =>
                    switch (a) {
                    | `FIRST => "FIRST"
                    | `SECOND => "SECOND"
                    | `THIRD => "THIRD"
                    }
                )(
                  b,
                ),
              )
            }
        )(
          inp##enum,
        ),
    };
  let make = (~arg, ()) => {
    "query": query,
    "variables":
      serializeVariables(
        {

          "arg": arg,
        }: t_variables,
      ),
    "parse": parse,
  }
  and makeVariables = (~arg, ()) =>
    serializeVariables(
      {

        "arg": arg,
      }: t_variables,
    )
  and makeInputObjectRecursiveInput =
      (~otherField=?, ~inner=?, ~enum=?, ()): t_variables_RecursiveInput => {

    "otherField": otherField,

    "inner": inner,

    "enum": enum,
  };
  let makeWithVariables = variables => {
    "query": query,
    "variables": serializeVariables(variables),
    "parse": parse,
  };
  let definition = (parse, query, serialize);
};

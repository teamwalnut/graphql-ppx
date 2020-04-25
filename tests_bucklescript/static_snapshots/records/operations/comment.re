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
    type t = {nonrecursiveInput: string};
    type t_variables = {arg: t_variables_NonrecursiveInput}
    and t_variables_NonrecursiveInput = {
      field: Js.Nullable.t(string),
      enum: Js.Nullable.t(string),
    };
  };
  let query = "query ($arg: NonrecursiveInput!)  {\nnonrecursiveInput(arg: $arg)  \n}\n";
  type t = {nonrecursiveInput: string};
  type t_variables = {arg: t_variables_NonrecursiveInput}
  and t_variables_NonrecursiveInput = {
    field: option(string),
    enum: option([ | `FIRST | `SECOND | `THIRD]),
  };
  let parse: Raw.t => t =
    (value) => (
      {

        nonrecursiveInput: {
          let value = (value: Raw.t).nonrecursiveInput;

          value;
        },
      }: t
    );
  let serialize: t => Raw.t =
    (value) => (
      {
        let nonrecursiveInput = {
          let value = (value: t).nonrecursiveInput;

          value;
        };
        {

          nonrecursiveInput: nonrecursiveInput,
        };
      }: Raw.t
    );
  let rec serializeVariables: t_variables => Raw.t_variables =
    inp => {

      arg:
        (a => Some(serializeInputObjectNonrecursiveInput(a)))(
          (inp: t_variables).arg,
        ),
    }
  and serializeInputObjectNonrecursiveInput:
    t_variables_NonrecursiveInput => Raw.t_variables_NonrecursiveInput =
    inp => {

      field:
        (
          a =>
            switch (a) {
            | None => Js.Nullable.undefined
            | Some(b) => Js.Nullable.return((a => a)(b))
            }
        )(
          (inp: t_variables_NonrecursiveInput).field,
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
          (inp: t_variables_NonrecursiveInput).enum,
        ),
    };
  let makeVariables = (~arg, ()) =>
    serializeVariables(
      {

        arg: arg,
      }: t_variables,
    )
  and makeInputObjectNonrecursiveInput =
      (~field=?, ~enum=?, ()): t_variables_NonrecursiveInput => {

    field,

    enum,
  };
  let definition = (parse, query, serialize);
};

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
    type t = {. "listsInput": string};
    type t_variables = {
      .
      "nullableOfNullable": Js.Nullable.t(array(Js.Nullable.t(string))),
      "nullableOfNonNullable": Js.Nullable.t(array(string)),
      "nonNullableOfNullable": array(Js.Nullable.t(string)),
      "nonNullableOfNonNullable": array(string),
    };
  };
  let query = "query ($nullableOfNullable: [String], $nullableOfNonNullable: [String!], $nonNullableOfNullable: [String]!, $nonNullableOfNonNullable: [String!]!)  {\nlistsInput(arg: {nullableOfNullable: $nullableOfNullable, nullableOfNonNullable: $nullableOfNonNullable, nonNullableOfNullable: $nonNullableOfNullable, nonNullableOfNonNullable: $nonNullableOfNonNullable})  \n}\n";
  type t = {. "listsInput": string};
  type t_variables = {
    .
    "nullableOfNullable": option(array(option(string))),
    "nullableOfNonNullable": option(array(string)),
    "nonNullableOfNullable": array(option(string)),
    "nonNullableOfNonNullable": array(string),
  };
  let parse: Raw.t => t =
    value => {
      "listsInput": {
        let value = value##listsInput;
        value;
      },
    };
  let serialize: t => Raw.t =
    value => {
      let listsInput = {
        let value = value##listsInput;

        value;
      };
      {

        "listsInput": listsInput,
      };
    };
  let serializeVariables: t_variables => Raw.t_variables =
    inp => {

      "nullableOfNullable":
        (
          a =>
            switch (a) {
            | None => Js.Nullable.undefined
            | Some(b) =>
              Js.Nullable.return(
                (
                  a =>
                    Array.map(
                      b =>
                        (
                          a =>
                            switch (a) {
                            | None => Js.Nullable.undefined
                            | Some(b) => Js.Nullable.return((a => a)(b))
                            }
                        )(
                          b,
                        ),
                      a,
                    )
                )(
                  b,
                ),
              )
            }
        )(
          inp##nullableOfNullable,
        ),

      "nullableOfNonNullable":
        (
          a =>
            switch (a) {
            | None => Js.Nullable.undefined
            | Some(b) =>
              Js.Nullable.return((a => Array.map(b => (a => a)(b), a))(b))
            }
        )(
          inp##nullableOfNonNullable,
        ),

      "nonNullableOfNullable":
        (
          a =>
            Array.map(
              b =>
                (
                  a =>
                    switch (a) {
                    | None => Js.Nullable.undefined
                    | Some(b) => Js.Nullable.return((a => a)(b))
                    }
                )(
                  b,
                ),
              a,
            )
        )(
          inp##nonNullableOfNullable,
        ),

      "nonNullableOfNonNullable":
        (a => Array.map(b => (a => a)(b), a))(
          inp##nonNullableOfNonNullable,
        ),
    };
  let makeVariables =
      (
        ~nullableOfNullable=?,
        ~nullableOfNonNullable=?,
        ~nonNullableOfNullable,
        ~nonNullableOfNonNullable,
        (),
      ) =>
    serializeVariables(
      {

        "nullableOfNullable": nullableOfNullable,

        "nullableOfNonNullable": nullableOfNonNullable,

        "nonNullableOfNullable": nonNullableOfNullable,

        "nonNullableOfNonNullable": nonNullableOfNonNullable,
      }: t_variables,
    );
  let definition = (parse, query, serialize);
};

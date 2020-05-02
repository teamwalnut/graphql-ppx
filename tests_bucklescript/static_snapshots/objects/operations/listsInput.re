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
    type t_variables = {. "arg": t_variables_ListsInput}
    and t_variables_ListsInput = {
      .
      "nullableOfNullable": Js.Nullable.t(array(Js.Nullable.t(string))),
      "nullableOfNonNullable": Js.Nullable.t(array(string)),
      "nonNullableOfNullable": array(Js.Nullable.t(string)),
      "nonNullableOfNonNullable": array(string),
    };
  };
  let query = "query ($arg: ListsInput!)  {\nlistsInput(arg: $arg)  \n}\n";
  type t = {. "listsInput": string};
  type t_variables = {. "arg": t_variables_ListsInput}
  and t_variables_ListsInput = {
    .
    "nullableOfNullable": option(array(option(string))),
    "nullableOfNonNullable": option(array(string)),
    "nonNullableOfNullable": array(option(string)),
    "nonNullableOfNonNullable": array(string),
  };
  let parse: Raw.t => t =
    value => {
      let listsInput = {
        let value = value##listsInput;
        value;
      };
      {

        "listsInput": listsInput,
      };
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

  let rec serializeVariables: t_variables => Raw.t_variables =
    inp => {

      "arg": (a => serializeInputObjectListsInput(a))(inp##arg),
    }

  and serializeInputObjectListsInput:
    t_variables_ListsInput => Raw.t_variables_ListsInput =
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
  let makeVariables = (~arg, ()) =>
    serializeVariables(
      {

        "arg": arg,
      }: t_variables,
    )
  and makeInputObjectListsInput =
      (
        ~nullableOfNullable=?,
        ~nullableOfNonNullable=?,
        ~nonNullableOfNullable,
        ~nonNullableOfNonNullable,
        (),
      )
      : t_variables_ListsInput => {

    "nullableOfNullable": nullableOfNullable,

    "nullableOfNonNullable": nullableOfNonNullable,

    "nonNullableOfNullable": nonNullableOfNullable,

    "nonNullableOfNonNullable": nonNullableOfNonNullable,
  };
  let definition = (parse, query, serialize);
};

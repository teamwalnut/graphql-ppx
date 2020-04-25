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
    type t = {listsInput: string};
    type t_variables = {arg: t_variables_ListsInput}
    and t_variables_ListsInput = {
      nullableOfNullable: Js.Nullable.t(array(Js.Nullable.t(string))),
      nullableOfNonNullable: Js.Nullable.t(array(string)),
      nonNullableOfNullable: array(Js.Nullable.t(string)),
      nonNullableOfNonNullable: array(string),
    };
  };
  let query = "query ($arg: ListsInput!)  {\nlistsInput(arg: $arg)  \n}\n";
  type t = {listsInput: string};
  type t_variables = {arg: t_variables_ListsInput}
  and t_variables_ListsInput = {
    nullableOfNullable: option(array(option(string))),
    nullableOfNonNullable: option(array(string)),
    nonNullableOfNullable: array(option(string)),
    nonNullableOfNonNullable: array(string),
  };
  let parse: Raw.t => t =
    (value) => (
      {

        listsInput: {
          let value = (value: Raw.t).listsInput;

          value;
        },
      }: t
    );
  let serialize: t => Raw.t =
    (value) => (
      {
        let listsInput = {
          let value = (value: t).listsInput;

          value;
        };
        {

          listsInput: listsInput,
        };
      }: Raw.t
    );
  let rec serializeVariables: t_variables => Raw.t_variables =
    inp => {

      arg:
        (a => Some(serializeInputObjectListsInput(a)))(
          (inp: t_variables).arg,
        ),
    }
  and serializeInputObjectListsInput:
    t_variables_ListsInput => Raw.t_variables_ListsInput =
    inp => {

      nullableOfNullable:
        (
          a =>
            switch (a) {
            | None => Js.Nullable.undefined
            | Some(b) =>
              Js.Nullable.return(
                (
                  a =>
                    a
                    |> Array.map(b =>
                         switch (
                           (
                             a =>
                               switch (a) {
                               | None => Js.Nullable.undefined
                               | Some(b) => Js.Nullable.return((a => a)(b))
                               }
                           )(
                             b,
                           )
                         ) {
                         | Some(c) => c
                         | None => Js.Nullable.null
                         }
                       )
                )(
                  b,
                ),
              )
            }
        )(
          (inp: t_variables_ListsInput).nullableOfNullable,
        ),

      nullableOfNonNullable:
        (
          a =>
            switch (a) {
            | None => Js.Nullable.undefined
            | Some(b) =>
              Js.Nullable.return(
                (
                  a =>
                    a
                    |> Array.map(b =>
                         switch ((a => a)(b)) {
                         | Some(c) => c
                         | None => Js.Nullable.null
                         }
                       )
                )(
                  b,
                ),
              )
            }
        )(
          (inp: t_variables_ListsInput).nullableOfNonNullable,
        ),

      nonNullableOfNullable:
        (
          a =>
            a
            |> Array.map(b =>
                 switch (
                   (
                     a =>
                       switch (a) {
                       | None => Js.Nullable.undefined
                       | Some(b) => Js.Nullable.return((a => a)(b))
                       }
                   )(
                     b,
                   )
                 ) {
                 | Some(c) => c
                 | None => Js.Nullable.null
                 }
               )
        )(
          (inp: t_variables_ListsInput).nonNullableOfNullable,
        ),

      nonNullableOfNonNullable:
        (
          a =>
            a
            |> Array.map(b =>
                 switch ((a => a)(b)) {
                 | Some(c) => c
                 | None => Js.Nullable.null
                 }
               )
        )(
          (inp: t_variables_ListsInput).nonNullableOfNonNullable,
        ),
    };
  let makeVariables = (~arg, ()) =>
    serializeVariables(
      {

        arg: arg,
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

    nullableOfNullable,

    nullableOfNonNullable,

    nonNullableOfNullable,

    nonNullableOfNonNullable,
  };
  let definition = (parse, query, serialize);
};

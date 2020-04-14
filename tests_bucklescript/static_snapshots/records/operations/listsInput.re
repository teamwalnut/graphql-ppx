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

        listsInput: {
          let value = (value: t).listsInput;

          value;
        },
      }: Raw.tt
    );
  let rec serializeVariables: t_variables => Js.Json.t =
    inp =>
      [|("arg", (a => Some(serializeInputObjectListsInput(a)))(inp.arg))|]
      |> Js.Array.filter(
           fun
           | (_, None) => false
           | (_, Some(_)) => true,
         )
      |> Js.Array.map(
           fun
           | (k, Some(v)) => (k, v)
           | (k, None) => (k, Js.Json.null),
         )
      |> Js.Dict.fromArray
      |> Js.Json.object_
  and serializeInputObjectListsInput: t_variables_ListsInput => Js.Json.t =
    inp =>
      [|
        (
          "nullableOfNullable",
          (
            a =>
              switch (a) {
              | None => None
              | Some(b) =>
                (
                  a =>
                    Some(
                      a
                      |> Array.map(b =>
                           switch (
                             (
                               a =>
                                 switch (a) {
                                 | None => None
                                 | Some(b) =>
                                   (a => Some(Js.Json.string(a)))(b)
                                 }
                             )(
                               b,
                             )
                           ) {
                           | Some(c) => c
                           | None => Js.Json.null
                           }
                         )
                      |> Js.Json.array,
                    )
                )(
                  b,
                )
              }
          )(
            inp.nullableOfNullable,
          ),
        ),
        (
          "nullableOfNonNullable",
          (
            a =>
              switch (a) {
              | None => None
              | Some(b) =>
                (
                  a =>
                    Some(
                      a
                      |> Array.map(b =>
                           switch ((a => Some(Js.Json.string(a)))(b)) {
                           | Some(c) => c
                           | None => Js.Json.null
                           }
                         )
                      |> Js.Json.array,
                    )
                )(
                  b,
                )
              }
          )(
            inp.nullableOfNonNullable,
          ),
        ),
        (
          "nonNullableOfNullable",
          (
            a =>
              Some(
                a
                |> Array.map(b =>
                     switch (
                       (
                         a =>
                           switch (a) {
                           | None => None
                           | Some(b) => (a => Some(Js.Json.string(a)))(b)
                           }
                       )(
                         b,
                       )
                     ) {
                     | Some(c) => c
                     | None => Js.Json.null
                     }
                   )
                |> Js.Json.array,
              )
          )(
            inp.nonNullableOfNullable,
          ),
        ),
        (
          "nonNullableOfNonNullable",
          (
            a =>
              Some(
                a
                |> Array.map(b =>
                     switch ((a => Some(Js.Json.string(a)))(b)) {
                     | Some(c) => c
                     | None => Js.Json.null
                     }
                   )
                |> Js.Json.array,
              )
          )(
            inp.nonNullableOfNonNullable,
          ),
        ),
      |]
      |> Js.Array.filter(
           fun
           | (_, None) => false
           | (_, Some(_)) => true,
         )
      |> Js.Array.map(
           fun
           | (k, Some(v)) => (k, v)
           | (k, None) => (k, Js.Json.null),
         )
      |> Js.Dict.fromArray
      |> Js.Json.object_;
  let makeVar = (~f, ~arg, ()) =>
    f(
      serializeVariables(
        {

          arg: arg,
        }: t_variables,
      ),
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
  let definition = (parse, query, makeVar);
  let makeVariables = makeVar(~f=f => f);
};

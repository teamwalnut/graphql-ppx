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
    type t = {
      __typename: string,
      nonrecursiveInput: string,
    };
  };
  let query = "query ($arg: NonrecursiveInput!)  {\n__typename  \nnonrecursiveInput(arg: $arg)  \n}\n";
  type t = {
    __typename: string,
    nonrecursiveInput: string,
  };
  type t_variables = {arg: t_variables_NonrecursiveInput}
  and t_variables_NonrecursiveInput = {
    field: option(string),
    enum: option([ | `FIRST | `SECOND | `THIRD]),
  };
  let parse: Raw.t => t =
    (value) => (
      {

        __typename: {
          let value = (value: Raw.t).__typename;

          value;
        },

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
        }
        and __typename = {
          let value = (value: t).__typename;

          value;
        };
        {

          __typename,

          nonrecursiveInput,
        };
      }: Raw.t
    );
  let rec serializeVariables: t_variables => Js.Json.t =
    inp =>
      [|
        (
          "arg",
          (a => Some(serializeInputObjectNonrecursiveInput(a)))(inp.arg),
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
      |> Js.Json.object_
  and serializeInputObjectNonrecursiveInput:
    t_variables_NonrecursiveInput => Js.Json.t =
    inp =>
      [|
        (
          "field",
          (
            a =>
              switch (a) {
              | None => None
              | Some(b) => (a => Some(Js.Json.string(a)))(b)
              }
          )(
            inp.field,
          ),
        ),
        (
          "enum",
          (
            a =>
              switch (a) {
              | None => None
              | Some(b) =>
                (
                  a =>
                    Some(
                      switch (a) {
                      | `FIRST => Js.Json.string("FIRST")
                      | `SECOND => Js.Json.string("SECOND")
                      | `THIRD => Js.Json.string("THIRD")
                      },
                    )
                )(
                  b,
                )
              }
          )(
            inp.enum,
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
  and makeInputObjectNonrecursiveInput =
      (~field=?, ~enum=?, ()): t_variables_NonrecursiveInput => {

    field,

    enum,
  };
  let definition = (parse, query, makeVar);
  let makeVariables = makeVar(~f=f => f);
};

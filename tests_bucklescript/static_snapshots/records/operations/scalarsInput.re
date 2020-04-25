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
    type t = {scalarsInput: string};
    type t_variables = {arg: t_variables_VariousScalarsInput}
    and t_variables_VariousScalarsInput = {
      nullableString: Js.Json.t(string),
      string,
      nullableInt: Js.Json.t(int),
      int,
      nullableFloat: Js.Json.t(float),
      float,
      nullableBoolean: Js.Json.t(bool),
      boolean: bool,
      nullableID: Js.Json.t(string),
      id: string,
    };
  };
  let query = "query ($arg: VariousScalarsInput!)  {\nscalarsInput(arg: $arg)  \n}\n";
  type t = {scalarsInput: string};
  type t_variables = {arg: t_variables_VariousScalarsInput}
  and t_variables_VariousScalarsInput = {
    nullableString: option(string),
    string,
    nullableInt: option(int),
    int,
    nullableFloat: option(float),
    float,
    nullableBoolean: option(bool),
    boolean: bool,
    nullableID: option(string),
    id: string,
  };
  let parse: Raw.t => t =
    (value) => (
      {

        scalarsInput: {
          let value = (value: Raw.t).scalarsInput;

          value;
        },
      }: t
    );
  let serialize: t => Raw.t =
    (value) => (
      {
        let scalarsInput = {
          let value = (value: t).scalarsInput;

          value;
        };
        {

          scalarsInput: scalarsInput,
        };
      }: Raw.t
    );
  let rec serializeVariables: t_variables => Js.Json.t =
    inp =>
      [|
        (
          "arg",
          (a => Some(serializeInputObjectVariousScalarsInput(a)))(inp.arg),
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
  and serializeInputObjectVariousScalarsInput:
    t_variables_VariousScalarsInput => Js.Json.t =
    inp =>
      [|
        (
          "nullableString",
          (
            a =>
              switch (a) {
              | None => None
              | Some(b) => (a => Some(Js.Json.string(a)))(b)
              }
          )(
            inp.nullableString,
          ),
        ),
        ("string", (a => Some(Js.Json.string(a)))(inp.string)),
        (
          "nullableInt",
          (
            a =>
              switch (a) {
              | None => None
              | Some(b) => (a => Some(Js.Json.number(float_of_int(a))))(b)
              }
          )(
            inp.nullableInt,
          ),
        ),
        ("int", (a => Some(Js.Json.number(float_of_int(a))))(inp.int)),
        (
          "nullableFloat",
          (
            a =>
              switch (a) {
              | None => None
              | Some(b) => (a => Some(Js.Json.number(a)))(b)
              }
          )(
            inp.nullableFloat,
          ),
        ),
        ("float", (a => Some(Js.Json.number(a)))(inp.float)),
        (
          "nullableBoolean",
          (
            a =>
              switch (a) {
              | None => None
              | Some(b) => (a => Some(Js.Json.boolean(a)))(b)
              }
          )(
            inp.nullableBoolean,
          ),
        ),
        ("boolean", (a => Some(Js.Json.boolean(a)))(inp.boolean)),
        (
          "nullableID",
          (
            a =>
              switch (a) {
              | None => None
              | Some(b) => (a => Some(Js.Json.string(a)))(b)
              }
          )(
            inp.nullableID,
          ),
        ),
        ("id", (a => Some(Js.Json.string(a)))(inp.id)),
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
  let makeVariables = (~arg, ()) =>
    serializeVariables(
      {

        arg: arg,
      }: t_variables,
    )
  and makeInputObjectVariousScalarsInput =
      (
        ~nullableString=?,
        ~string,
        ~nullableInt=?,
        ~int,
        ~nullableFloat=?,
        ~float,
        ~nullableBoolean=?,
        ~boolean,
        ~nullableID=?,
        ~id,
        (),
      )
      : t_variables_VariousScalarsInput => {

    nullableString,

    string,

    nullableInt,

    int,

    nullableFloat,

    float,

    nullableBoolean,

    boolean,

    nullableID,

    id,
  };
  let definition = (parse, query, serialize);
};

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
    type t_variables = {
      nullableString: Js.Nullable.t(string),
      string,
      nullableInt: Js.Nullable.t(int),
      int,
      nullableFloat: Js.Nullable.t(float),
      float,
      nullableBoolean: Js.Nullable.t(bool),
      boolean: bool,
      nullableID: Js.Nullable.t(string),
      id: string,
    };
  };
  let query = "query ($nullableString: String, $string: String!, $nullableInt: Int, $int: Int!, $nullableFloat: Float, $float: Float!, $nullableBoolean: Boolean, $boolean: Boolean!, $nullableID: ID, $id: ID!)  {\nscalarsInput(arg: {nullableString: $nullableString, string: $string, nullableInt: $nullableInt, int: $int, nullableFloat: $nullableFloat, float: $float, nullableBoolean: $nullableBoolean, boolean: $boolean, nullableID: $nullableID, id: $id})  \n}\n";
  type t = {scalarsInput: string};
  type t_variables = {
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
  let serializeVariables: t_variables => Raw.t_variables =
    inp => {

      nullableString:
        (
          a =>
            switch (a) {
            | None => Js.Nullable.undefined
            | Some(b) => Js.Nullable.return((a => a)(b))
            }
        )(
          (inp: t_variables).nullableString,
        ),

      string: (a => a)((inp: t_variables).string),

      nullableInt:
        (
          a =>
            switch (a) {
            | None => Js.Nullable.undefined
            | Some(b) => Js.Nullable.return((a => a)(b))
            }
        )(
          (inp: t_variables).nullableInt,
        ),

      int: (a => a)((inp: t_variables).int),

      nullableFloat:
        (
          a =>
            switch (a) {
            | None => Js.Nullable.undefined
            | Some(b) => Js.Nullable.return((a => a)(b))
            }
        )(
          (inp: t_variables).nullableFloat,
        ),

      float: (a => a)((inp: t_variables).float),

      nullableBoolean:
        (
          a =>
            switch (a) {
            | None => Js.Nullable.undefined
            | Some(b) => Js.Nullable.return((a => a)(b))
            }
        )(
          (inp: t_variables).nullableBoolean,
        ),

      boolean: (a => a)((inp: t_variables).boolean),

      nullableID:
        (
          a =>
            switch (a) {
            | None => Js.Nullable.undefined
            | Some(b) => Js.Nullable.return((a => a)(b))
            }
        )(
          (inp: t_variables).nullableID,
        ),

      id: (a => a)((inp: t_variables).id),
    };
  let makeVariables =
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
      ) =>
    serializeVariables(
      {

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
      }: t_variables,
    );
  let definition = (parse, query, serialize);
};

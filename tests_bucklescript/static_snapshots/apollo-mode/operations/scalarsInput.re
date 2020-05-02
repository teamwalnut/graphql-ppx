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
        let scalarsInput = {
          let value = (value: Raw.t).scalarsInput;
          value;
        };
        {

          scalarsInput: scalarsInput,
        };
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

  let rec serializeVariables: t_variables => Raw.t_variables =
    inp => {

      arg:
        (a => serializeInputObjectVariousScalarsInput(a))(
          (inp: t_variables).arg,
        ),
    }

  and serializeInputObjectVariousScalarsInput:
    t_variables_VariousScalarsInput => Raw.t_variables_VariousScalarsInput =
    inp => {

      nullableString:
        (
          a =>
            switch (a) {
            | None => Js.Nullable.undefined
            | Some(b) => Js.Nullable.return((a => a)(b))
            }
        )(
          (inp: t_variables_VariousScalarsInput).nullableString,
        ),

      string: (a => a)((inp: t_variables_VariousScalarsInput).string),

      nullableInt:
        (
          a =>
            switch (a) {
            | None => Js.Nullable.undefined
            | Some(b) => Js.Nullable.return((a => a)(b))
            }
        )(
          (inp: t_variables_VariousScalarsInput).nullableInt,
        ),

      int: (a => a)((inp: t_variables_VariousScalarsInput).int),

      nullableFloat:
        (
          a =>
            switch (a) {
            | None => Js.Nullable.undefined
            | Some(b) => Js.Nullable.return((a => a)(b))
            }
        )(
          (inp: t_variables_VariousScalarsInput).nullableFloat,
        ),

      float: (a => a)((inp: t_variables_VariousScalarsInput).float),

      nullableBoolean:
        (
          a =>
            switch (a) {
            | None => Js.Nullable.undefined
            | Some(b) => Js.Nullable.return((a => a)(b))
            }
        )(
          (inp: t_variables_VariousScalarsInput).nullableBoolean,
        ),

      boolean: (a => a)((inp: t_variables_VariousScalarsInput).boolean),

      nullableID:
        (
          a =>
            switch (a) {
            | None => Js.Nullable.undefined
            | Some(b) => Js.Nullable.return((a => a)(b))
            }
        )(
          (inp: t_variables_VariousScalarsInput).nullableID,
        ),

      id: (a => a)((inp: t_variables_VariousScalarsInput).id),
    };
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

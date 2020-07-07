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
  [@ocaml.warning "-32"];
  module Raw = {
    type t = {nonrecursiveInput: string};
    type t_variables = {arg: t_variables_NonrecursiveInput}
    and t_variables_NonrecursiveInput = {
      nonNullableField: string,
      nullableArray: Js.Nullable.t(array(Js.Nullable.t(string))),
      field: Js.Nullable.t(string),
      enum: Js.Nullable.t(string),
      embeddedInput:
        Js.Nullable.t(array(Js.Nullable.t(t_variables_EmbeddedInput))),
      custom: Js.Nullable.t(Js.Json.t),
    }
    and t_variables_EmbeddedInput = {field: Js.Nullable.t(string)};
  };
  type t = {nonrecursiveInput: string};
  /**The GraphQL query string*/
  let query = "query ($arg: NonrecursiveInput!)  {\nnonrecursiveInput(arg: $arg)  \n}\n";
  type t_variables = {arg: t_variables_NonrecursiveInput}
  and t_variables_NonrecursiveInput = {
    nonNullableField: string,
    nullableArray: option(array(option(string))),
    field: option(string),
    enum: option([ | `FIRST | `SECOND | `THIRD]),
    embeddedInput: option(array(option(t_variables_EmbeddedInput))),
    custom: option(Js.Json.t),
  }
  and t_variables_EmbeddedInput = {field: option(string)};
  /**Parse the JSON GraphQL data to ReasonML data types*/
  let parse = (value: Raw.t): t => (
    {
      nonrecursiveInput: {
        let value = (value: Raw.t).nonrecursiveInput;
        value;
      },
    }: t
  );
  /**Serialize the ReasonML GraphQL data that was parsed using the parse function back to the original JSON compatible data */
  let serialize = (value: t): Raw.t => (
    {
      let nonrecursiveInput = {
        let value = (value: t).nonrecursiveInput;
        value;
      };
      {nonrecursiveInput: nonrecursiveInput};
    }: Raw.t
  );
  let rec serializeVariables: t_variables => Raw.t_variables =
    inp => {
      arg:
        (a => serializeInputObjectNonrecursiveInput(a))(
          (inp: t_variables).arg,
        ),
    }
  and serializeInputObjectNonrecursiveInput:
    t_variables_NonrecursiveInput => Raw.t_variables_NonrecursiveInput =
    inp => {
      nonNullableField:
        (a => a)((inp: t_variables_NonrecursiveInput).nonNullableField),
      nullableArray:
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
          (inp: t_variables_NonrecursiveInput).nullableArray,
        ),
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
      embeddedInput:
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
                            | Some(b) =>
                              Js.Nullable.return(
                                (a => serializeInputObjectEmbeddedInput(a))(
                                  b,
                                ),
                              )
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
          (inp: t_variables_NonrecursiveInput).embeddedInput,
        ),
      custom:
        (
          a =>
            switch (a) {
            | None => Js.Nullable.undefined
            | Some(b) => Js.Nullable.return((a => a)(b))
            }
        )(
          (inp: t_variables_NonrecursiveInput).custom,
        ),
    }
  and serializeInputObjectEmbeddedInput:
    t_variables_EmbeddedInput => Raw.t_variables_EmbeddedInput =
    inp => {
      field:
        (
          a =>
            switch (a) {
            | None => Js.Nullable.undefined
            | Some(b) => Js.Nullable.return((a => a)(b))
            }
        )(
          (inp: t_variables_EmbeddedInput).field,
        ),
    };
  let makeVariables = (~arg, ()) =>
    serializeVariables({arg: arg}: t_variables)
  and makeInputObjectNonrecursiveInput =
      (
        ~nonNullableField,
        ~nullableArray=?,
        ~field=?,
        ~enum=?,
        ~embeddedInput=?,
        ~custom=?,
        (),
      )
      : t_variables_NonrecursiveInput => {
    nonNullableField,
    nullableArray,
    field,
    enum,
    embeddedInput,
    custom,
  }
  and makeInputObjectEmbeddedInput = (~field=?, ()): t_variables_EmbeddedInput => {
    field: field,
  };
  external unsafe_fromJson: Js.Json.t => Raw.t = "%identity";
  external toJson: Raw.t => Js.Json.t = "%identity";
  external variablesToJson: Raw.t_variables => Js.Json.t = "%identity";
  module Z__INTERNAL = {
    type nonrec _graphql_arg_100;
    /**Variable **$arg** has the following graphql type:

```
NonrecursiveInput {
  nonNullableField: String!
  nullableArray: [String]
  field: String
  enum: SampleField
  embeddedInput: [EmbeddedInput]
  custom: CustomScalar
}!
```*/
    let _graphql_arg_100: _graphql_arg_100 = Obj.magic(0);
    type nonrec _graphql_arg_95;
    /**Argument **arg** on field **nonrecursiveInput** has the following graphql type:

```
NonrecursiveInput {
  nonNullableField: String!
  nullableArray: [String]
  field: String
  enum: SampleField
  embeddedInput: [EmbeddedInput]
  custom: CustomScalar
}!
```*/
    let _graphql_arg_95: _graphql_arg_95 = Obj.magic(0);
    type root = t;
    type nonrec _graphql_NonrecursiveInput_49;
    /**```
NonrecursiveInput {
  nonNullableField: String!
  nullableArray: [String]
  field: String
  enum: SampleField
  embeddedInput: [EmbeddedInput]
  custom: CustomScalar
}
```*/
    let _graphql_NonrecursiveInput_49: _graphql_NonrecursiveInput_49 =
      Obj.magic(0);
    type nonrec graphql_module;
    /****--- graphql-ppx module ---**

The contents of this module are automatically generated by `graphql-ppx`.
The following is simply an overview of the most important variables and types that you can access from this module.

```
module MyQuery {
  /**
  The GraphQL query string
  */
  let query: string;

  /**
  This is the main type of the result you will get back.
  You can hover above the identifier key (e.g. query or mutation) to see the fully generated type for your module.
  */
  type t;

  /**
  Parse the JSON GraphQL data to ReasonML data types
  */
  let parse: Raw.t => t;

  /**
  Serialize the ReasonML GraphQL data that was parsed using the parse function back to the original JSON compatible data
  */
  let serialize: t => Raw.t;

  /**
  This is the JSON compatible type of the GraphQL data.
  It should not be necessary to access the types inside for normal use cases.
  */
  module Raw: { type t; };
}
```*/
    let graphql_module: graphql_module = Obj.magic(0);
  };
};

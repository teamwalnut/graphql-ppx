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
    type t = {. "nonrecursiveInput": string};
    type t_variables = {. "arg": t_variables_NonrecursiveInput}
    and t_variables_NonrecursiveInput = {
      .
      "nonNullableField": string,
      "nullableArray": Js.Nullable.t(array(Js.Nullable.t(string))),
      "field": Js.Nullable.t(string),
      "enum": Js.Nullable.t(string),
      "embeddedInput":
        Js.Nullable.t(array(Js.Nullable.t(t_variables_EmbeddedInput))),
      "custom": Js.Nullable.t(Js.Json.t),
    }
    and t_variables_EmbeddedInput = {. "field": Js.Nullable.t(string)};
  };
  let query = "query ($arg: NonrecursiveInput!)  {\nnonrecursiveInput(arg: $arg)  \n}\n";
  type t = {. "nonrecursiveInput": string};
  type t_variables = {. "arg": t_variables_NonrecursiveInput}
  and t_variables_NonrecursiveInput = {
    .
    "nonNullableField": string,
    "nullableArray": option(array(option(string))),
    "field": option(string),
    "enum": option([ | `FIRST | `SECOND | `THIRD]),
    "embeddedInput": option(array(option(t_variables_EmbeddedInput))),
    "custom": option(Js.Json.t),
  }
  and t_variables_EmbeddedInput = {. "field": option(string)};
  let parse: Raw.t => t =
    value => {
      let nonrecursiveInput = {
        let value = value##nonrecursiveInput;
        value;
      };
      {"nonrecursiveInput": nonrecursiveInput};
    };
  let serialize: t => Raw.t =
    value => {
      let nonrecursiveInput = {
        let value = value##nonrecursiveInput;
        value;
      };
      {"nonrecursiveInput": nonrecursiveInput};
    };
  let rec serializeVariables: t_variables => Raw.t_variables =
    inp => {
      "arg": (a => serializeInputObjectNonrecursiveInput(a))(inp##arg),
    }
  and serializeInputObjectNonrecursiveInput:
    t_variables_NonrecursiveInput => Raw.t_variables_NonrecursiveInput =
    inp => {
      "nonNullableField": (a => a)(inp##nonNullableField),
      "nullableArray":
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
          inp##nullableArray,
        ),
      "field":
        (
          a =>
            switch (a) {
            | None => Js.Nullable.undefined
            | Some(b) => Js.Nullable.return((a => a)(b))
            }
        )(
          inp##field,
        ),
      "enum":
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
          inp##enum,
        ),
      "embeddedInput":
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
          inp##embeddedInput,
        ),
      "custom":
        (
          a =>
            switch (a) {
            | None => Js.Nullable.undefined
            | Some(b) => Js.Nullable.return((a => a)(b))
            }
        )(
          inp##custom,
        ),
    }
  and serializeInputObjectEmbeddedInput:
    t_variables_EmbeddedInput => Raw.t_variables_EmbeddedInput =
    inp => {
      "field":
        (
          a =>
            switch (a) {
            | None => Js.Nullable.undefined
            | Some(b) => Js.Nullable.return((a => a)(b))
            }
        )(
          inp##field,
        ),
    };
  let makeVariables = (~arg, ()) =>
    serializeVariables({"arg": arg}: t_variables)
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
    "nonNullableField": nonNullableField,
    "nullableArray": nullableArray,
    "field": field,
    "enum": enum,
    "embeddedInput": embeddedInput,
    "custom": custom,
  }
  and makeInputObjectEmbeddedInput = (~field=?, ()): t_variables_EmbeddedInput => {
    "field": field,
  };
  let definition = (parse, query, serialize);
  module Z__INTERNAL = {
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
    /****--- GraphQL PPX Module ---**

The contents of this module are automatically generated by `graphql-ppx`.
The following is simply an overview of the most important variables and types that you can access from this module.

```
module MyQuery {
  // This is the stringified representation of your query, which gets sent to the server.
  let query: string;

  // This is the main type of the result you will get back.
  // You can hover above the identifier key (e.g. query or mutation) to see the fully generated type for your module.
  type t;

  // This function turns your raw result from the server into the reason/ocaml representation of that result.
  // Depending on your graphql client library, this process should happen automatically for you.
  let parse: Raw.t => t;

  // This function will prepare your data for sending it back to the server.
  // Depending on your graphql client library, this process should happen automatically for you.
  let serialize: t => Raw.t;

  // The definition tuple is primarily used to interact with client libraries.
  // The types are equivalent to: (parse, query, serialize).
  // Your client library will use these values to provide the properly parsed / serialized data for you.
  let definition: (
    Raw.t => t,
    string,
    t => Raw.t
  );

  // This is the representation of your raw result coming from the server.
  // It should not be necessary to access the types inside for normal use cases.
  module Raw: { type t; };
}
```*/
    let graphql_module: graphql_module = Obj.magic(0);
  };
};

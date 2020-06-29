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
    type t_lists = {
      nullableOfNullable: Js.Nullable.t(array(Js.Nullable.t(string))),
      nullableOfNonNullable: Js.Nullable.t(array(string)),
      nonNullableOfNullable: array(Js.Nullable.t(string)),
      nonNullableOfNonNullable: array(string),
    };
    type t = {lists: t_lists};
    type t_variables = unit;
  };
  /**The GraphQL query string*/
  let query = [%raw
    "require(\"gql\")`\n  query   {\n    lists  {\n      nullableOfNullable\n      nullableOfNonNullable\n      nonNullableOfNullable\n      nonNullableOfNonNullable\n    }\n  }\n`"
  ];
  type t_lists = {
    nullableOfNullable: option(array(option(string))),
    nullableOfNonNullable: option(array(string)),
    nonNullableOfNullable: array(option(string)),
    nonNullableOfNonNullable: array(string),
  };
  type t = {lists: t_lists};
  type t_variables = unit;
  /**Parse the JSON GraphQL data to ReasonML data types*/
  let parse = (value: Raw.t): t => (
    {
      lists: {
        let value = (value: Raw.t).lists;
        (
          {
            nullableOfNullable: {
              let value = (value: Raw.t_lists).nullableOfNullable;
              switch (Js.toOption(value)) {
              | Some(value) =>
                Some(
                  value
                  |> Js.Array.map(value =>
                       switch (Js.toOption(value)) {
                       | Some(value) => Some(value)
                       | None => None
                       }
                     ),
                )
              | None => None
              };
            },
            nullableOfNonNullable: {
              let value = (value: Raw.t_lists).nullableOfNonNullable;
              switch (Js.toOption(value)) {
              | Some(value) => Some(value |> Js.Array.map(value => value))
              | None => None
              };
            },
            nonNullableOfNullable: {
              let value = (value: Raw.t_lists).nonNullableOfNullable;
              value
              |> Js.Array.map(value =>
                   switch (Js.toOption(value)) {
                   | Some(value) => Some(value)
                   | None => None
                   }
                 );
            },
            nonNullableOfNonNullable: {
              let value = (value: Raw.t_lists).nonNullableOfNonNullable;
              value |> Js.Array.map(value => value);
            },
          }: t_lists
        );
      },
    }: t
  );
  /**Serialize the ReasonML GraphQL data that was parsed using the parse function back to the original JSON compatible data */
  let serialize = (value: t): Raw.t => (
    {
      let lists = {
        let value = (value: t).lists;
        (
          {
            let nonNullableOfNonNullable = {
              let value = (value: t_lists).nonNullableOfNonNullable;
              value |> Js.Array.map(value => value);
            }
            and nonNullableOfNullable = {
              let value = (value: t_lists).nonNullableOfNullable;
              value
              |> Js.Array.map(value =>
                   switch (value) {
                   | Some(value) => Js.Nullable.return(value)
                   | None => Js.Nullable.null
                   }
                 );
            }
            and nullableOfNonNullable = {
              let value = (value: t_lists).nullableOfNonNullable;
              switch (value) {
              | Some(value) =>
                Js.Nullable.return(value |> Js.Array.map(value => value))
              | None => Js.Nullable.null
              };
            }
            and nullableOfNullable = {
              let value = (value: t_lists).nullableOfNullable;
              switch (value) {
              | Some(value) =>
                Js.Nullable.return(
                  value
                  |> Js.Array.map(value =>
                       switch (value) {
                       | Some(value) => Js.Nullable.return(value)
                       | None => Js.Nullable.null
                       }
                     ),
                )
              | None => Js.Nullable.null
              };
            };
            {
              nullableOfNullable,
              nullableOfNonNullable,
              nonNullableOfNullable,
              nonNullableOfNonNullable,
            };
          }: Raw.t_lists
        );
      };
      {lists: lists};
    }: Raw.t
  );
  let serializeVariables = () => ();
  let makeVariables = () => ();
  let makeDefaultVariables = () => makeVariables();
  external unsafe_fromJson: Js.Json.t => Raw.t = "%identity";
  external toJson: Raw.t => Js.Json.t = "%identity";
  external variablesToJson: Raw.t_variables => Js.Json.t = "%identity";
  module Z__INTERNAL = {
    type root = t;
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

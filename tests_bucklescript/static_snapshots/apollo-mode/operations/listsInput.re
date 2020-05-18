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
module MyQuery' = {
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
        {listsInput: listsInput};
      }: Raw.t
    );
  let rec serializeVariables: t_variables => Raw.t_variables =
    inp => {
      arg: (a => serializeInputObjectListsInput(a))((inp: t_variables).arg),
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
          (inp: t_variables_ListsInput).nullableOfNullable,
        ),
      nullableOfNonNullable:
        (
          a =>
            switch (a) {
            | None => Js.Nullable.undefined
            | Some(b) =>
              Js.Nullable.return((a => Array.map(b => (a => a)(b), a))(b))
            }
        )(
          (inp: t_variables_ListsInput).nullableOfNonNullable,
        ),
      nonNullableOfNullable:
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
          (inp: t_variables_ListsInput).nonNullableOfNullable,
        ),
      nonNullableOfNonNullable:
        (a => Array.map(b => (a => a)(b), a))(
          (inp: t_variables_ListsInput).nonNullableOfNonNullable,
        ),
    };
  let makeVariables = (~arg, ()) =>
    serializeVariables({arg: arg}: t_variables)
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
  module Z__INTERNAL = {
    type nonrec _graphql_arg_82;
    /**Variable **$arg** has the following graphql type:

```
ListsInput {
  nullableOfNullable: [String]
  nullableOfNonNullable: [String!]
  nonNullableOfNullable: [String]!
  nonNullableOfNonNullable: [String!]!
}!
```*/
    let _graphql_arg_82: _graphql_arg_82 = Obj.magic(0);
    type nonrec _graphql_arg_77;
    /**Argument **arg** on field **listsInput** has the following graphql type:

```
ListsInput {
  nullableOfNullable: [String]
  nullableOfNonNullable: [String!]
  nonNullableOfNullable: [String]!
  nonNullableOfNonNullable: [String!]!
}!
```*/
    let _graphql_arg_77: _graphql_arg_77 = Obj.magic(0);
    type root = t;
    type nonrec _graphql_ListsInput_47;
    /**```
ListsInput {
  nullableOfNullable: [String]
  nullableOfNonNullable: [String!]
  nonNullableOfNullable: [String]!
  nonNullableOfNonNullable: [String!]!
}
```*/
    let _graphql_ListsInput_47: _graphql_ListsInput_47 = Obj.magic(0);
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
module MyQuery = {
  include MyQuery';
  let self: module GraphQL_PPX.Query = (module MyQuery');
};

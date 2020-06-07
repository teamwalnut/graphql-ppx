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
    type t_v1 = {
      __typename: string,
      nullableString: Js.Nullable.t(string),
      string: Js.Nullable.t(string),
    };
    type t_v2 = {
      __typename: string,
      nullableString: Js.Nullable.t(string),
      string: Js.Nullable.t(string),
    };
    type t = {
      v1: t_v1,
      v2: t_v2,
    };
    type t_variables = {var: bool};
  };
  /**The GraphQL query string*/
  let query = "query ($var: Boolean!)  {\nv1: variousScalars  {\n__typename  \nnullableString @skip(if: $var) \nstring @skip(if: $var) \n}\n\nv2: variousScalars  {\n__typename  \nnullableString @include(if: $var) \nstring @include(if: $var) \n}\n\n}\n";
  type t_v1 = {
    __typename: string,
    nullableString: option(string),
    string: option(string),
  };
  type t_v2 = {
    __typename: string,
    nullableString: option(string),
    string: option(string),
  };
  type t = {
    v1: t_v1,
    v2: t_v2,
  };
  type t_variables = {var: bool};
  /**Parse the JSON GraphQL data to ReasonML data types*/
  let parse = (value: Raw.t): t => (
    {
      v1: {
        let value = (value: Raw.t).v1;
        (
          {
            __typename: {
              let value = (value: Raw.t_v1).__typename;
              value;
            },
            nullableString: {
              let value = (value: Raw.t_v1).nullableString;
              switch (Js.toOption(value)) {
              | Some(value) => Some(value)
              | None => None
              };
            },
            string: {
              let value = (value: Raw.t_v1).string;
              switch (Js.toOption(value)) {
              | Some(value) => Some(value)
              | None => None
              };
            },
          }: t_v1
        );
      },
      v2: {
        let value = (value: Raw.t).v2;
        (
          {
            __typename: {
              let value = (value: Raw.t_v2).__typename;
              value;
            },
            nullableString: {
              let value = (value: Raw.t_v2).nullableString;
              switch (Js.toOption(value)) {
              | Some(value) => Some(value)
              | None => None
              };
            },
            string: {
              let value = (value: Raw.t_v2).string;
              switch (Js.toOption(value)) {
              | Some(value) => Some(value)
              | None => None
              };
            },
          }: t_v2
        );
      },
    }: t
  );
  /**Serialize the ReasonML GraphQL data that was parsed using the parse function back to the original JSON compatible data */
  let serialize = (value: t): Raw.t => (
    {
      let v2 = {
        let value = (value: t).v2;
        (
          {
            let string = {
              let value = (value: t_v2).string;
              switch (value) {
              | Some(value) => Js.Nullable.return(value)
              | None => Js.Nullable.null
              };
            }
            and nullableString = {
              let value = (value: t_v2).nullableString;
              switch (value) {
              | Some(value) => Js.Nullable.return(value)
              | None => Js.Nullable.null
              };
            }
            and __typename = {
              let value = (value: t_v2).__typename;
              value;
            };
            {__typename, nullableString, string};
          }: Raw.t_v2
        );
      }
      and v1 = {
        let value = (value: t).v1;
        (
          {
            let string = {
              let value = (value: t_v1).string;
              switch (value) {
              | Some(value) => Js.Nullable.return(value)
              | None => Js.Nullable.null
              };
            }
            and nullableString = {
              let value = (value: t_v1).nullableString;
              switch (value) {
              | Some(value) => Js.Nullable.return(value)
              | None => Js.Nullable.null
              };
            }
            and __typename = {
              let value = (value: t_v1).__typename;
              value;
            };
            {__typename, nullableString, string};
          }: Raw.t_v1
        );
      };
      {v1, v2};
    }: Raw.t
  );
  let serializeVariables: t_variables => Raw.t_variables =
    inp => {var: (a => a)((inp: t_variables).var)};
  let makeVariables = (~var, ()) =>
    serializeVariables({var: var}: t_variables);
  external unsafe_fromJson: Js.Json.t => Raw.t = "%identity";
  external toJson: Raw.t => Js.Json.t = "%identity";
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

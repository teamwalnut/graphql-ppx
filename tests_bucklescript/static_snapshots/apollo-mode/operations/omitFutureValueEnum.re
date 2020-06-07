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
module Normal = {
  module Raw = {
    type t_mutationWithError_errors_field = string;
    type t_mutationWithError_errors = {
      __typename: string,
      message: string,
      field: t_mutationWithError_errors_field,
    };
    type t_mutationWithError = {
      __typename: string,
      errors: Js.Nullable.t(array(t_mutationWithError_errors)),
    };
    type t = {mutationWithError: t_mutationWithError};
    type t_variables = unit;
  };
  /**The GraphQL query string*/
  let query = "mutation   {\nmutationWithError  {\n__typename  \nerrors  {\n__typename  \nmessage  \nfield  \n}\n\n}\n\n}\n";
  type t_mutationWithError_errors_field = [
    | `FutureAddedValue(string)
    | `FIRST
    | `SECOND
    | `THIRD
  ];
  type t_mutationWithError_errors = {
    __typename: string,
    message: string,
    field: t_mutationWithError_errors_field,
  };
  type t_mutationWithError = {
    __typename: string,
    errors: option(array(t_mutationWithError_errors)),
  };
  type t = {mutationWithError: t_mutationWithError};
  type t_variables = unit;
  /**Parse the JSON GraphQL data to ReasonML data types*/
  let parse = (value: Raw.t): t => (
    {
      mutationWithError: {
        let value = (value: Raw.t).mutationWithError;
        (
          {
            __typename: {
              let value = (value: Raw.t_mutationWithError).__typename;
              value;
            },
            errors: {
              let value = (value: Raw.t_mutationWithError).errors;
              switch (Js.toOption(value)) {
              | Some(value) =>
                Some(
                  value
                  |> Js.Array.map((value) =>
                       (
                         {
                           __typename: {
                             let value =
                               (value: Raw.t_mutationWithError_errors).
                                 __typename;
                             value;
                           },
                           message: {
                             let value =
                               (value: Raw.t_mutationWithError_errors).message;
                             value;
                           },
                           field: {
                             let value =
                               (value: Raw.t_mutationWithError_errors).field;
                             switch (Obj.magic(value: string)) {
                             | "FIRST" => `FIRST
                             | "SECOND" => `SECOND
                             | "THIRD" => `THIRD
                             | other => `FutureAddedValue(other)
                             };
                           },
                         }: t_mutationWithError_errors
                       )
                     ),
                )
              | None => None
              };
            },
          }: t_mutationWithError
        );
      },
    }: t
  );
  /**Serialize the ReasonML GraphQL data that was parsed using the parse function back to the original JSON compatible data */
  let serialize = (value: t): Raw.t => (
    {
      let mutationWithError = {
        let value = (value: t).mutationWithError;
        (
          {
            let errors = {
              let value = (value: t_mutationWithError).errors;
              switch (value) {
              | Some(value) =>
                Js.Nullable.return(
                  value
                  |> Js.Array.map((value) =>
                       (
                         {
                           let field = {
                             let value =
                               (value: t_mutationWithError_errors).field;
                             switch (value) {
                             | `FIRST => "FIRST"
                             | `SECOND => "SECOND"
                             | `THIRD => "THIRD"
                             | `FutureAddedValue(other) => other
                             };
                           }
                           and message = {
                             let value =
                               (value: t_mutationWithError_errors).message;
                             value;
                           }
                           and __typename = {
                             let value =
                               (value: t_mutationWithError_errors).__typename;
                             value;
                           };
                           {__typename, message, field};
                         }: Raw.t_mutationWithError_errors
                       )
                     ),
                )
              | None => Js.Nullable.null
              };
            }
            and __typename = {
              let value = (value: t_mutationWithError).__typename;
              value;
            };
            {__typename, errors};
          }: Raw.t_mutationWithError
        );
      };
      {mutationWithError: mutationWithError};
    }: Raw.t
  );
  let makeVariables = () => ();
  let makeDefaultVariables = () => makeVariables();
  external unsafe_fromJson: Js.Json.t => Raw.t = "%identity";
  external toJson: Raw.t => Js.Json.t = "%identity";
  module Z__INTERNAL = {
    type root = t;
    type nonrec graphql_module;
    /****--- graphql-ppx module ---**

The contents of this module are automatically generated by `graphql-ppx`.
The following is simply an overview of the most important variables and types that you can access from this module.

```
module Normal {
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
module ByConfig = {
  module Raw = {
    type t_mutationWithError_errors_field = string;
    type t_mutationWithError_errors = {
      __typename: string,
      message: string,
      field: t_mutationWithError_errors_field,
    };
    type t_mutationWithError = {
      __typename: string,
      errors: Js.Nullable.t(array(t_mutationWithError_errors)),
    };
    type t = {mutationWithError: t_mutationWithError};
    type t_variables = unit;
  };
  /**The GraphQL query string*/
  let query = "mutation   {\nmutationWithError  {\n__typename  \nerrors  {\n__typename  \nmessage  \nfield  \n}\n\n}\n\n}\n";
  type t_mutationWithError_errors_field = [ | `FIRST | `SECOND | `THIRD];
  type t_mutationWithError_errors = {
    __typename: string,
    message: string,
    field: t_mutationWithError_errors_field,
  };
  type t_mutationWithError = {
    __typename: string,
    errors: option(array(t_mutationWithError_errors)),
  };
  type t = {mutationWithError: t_mutationWithError};
  type t_variables = unit;
  /**Parse the JSON GraphQL data to ReasonML data types*/
  let parse = (value: Raw.t): t => (
    {
      mutationWithError: {
        let value = (value: Raw.t).mutationWithError;
        (
          {
            __typename: {
              let value = (value: Raw.t_mutationWithError).__typename;
              value;
            },
            errors: {
              let value = (value: Raw.t_mutationWithError).errors;
              switch (Js.toOption(value)) {
              | Some(value) =>
                Some(
                  value
                  |> Js.Array.map((value) =>
                       (
                         {
                           __typename: {
                             let value =
                               (value: Raw.t_mutationWithError_errors).
                                 __typename;
                             value;
                           },
                           message: {
                             let value =
                               (value: Raw.t_mutationWithError_errors).message;
                             value;
                           },
                           field: {
                             let value =
                               (value: Raw.t_mutationWithError_errors).field;
                             switch (Obj.magic(value: string)) {
                             | "FIRST" => `FIRST
                             | "SECOND" => `SECOND
                             | "THIRD" => `THIRD
                             | _ => raise(Not_found)
                             };
                           },
                         }: t_mutationWithError_errors
                       )
                     ),
                )
              | None => None
              };
            },
          }: t_mutationWithError
        );
      },
    }: t
  );
  /**Serialize the ReasonML GraphQL data that was parsed using the parse function back to the original JSON compatible data */
  let serialize = (value: t): Raw.t => (
    {
      let mutationWithError = {
        let value = (value: t).mutationWithError;
        (
          {
            let errors = {
              let value = (value: t_mutationWithError).errors;
              switch (value) {
              | Some(value) =>
                Js.Nullable.return(
                  value
                  |> Js.Array.map((value) =>
                       (
                         {
                           let field = {
                             let value =
                               (value: t_mutationWithError_errors).field;
                             switch (value) {
                             | `FIRST => "FIRST"
                             | `SECOND => "SECOND"
                             | `THIRD => "THIRD"
                             };
                           }
                           and message = {
                             let value =
                               (value: t_mutationWithError_errors).message;
                             value;
                           }
                           and __typename = {
                             let value =
                               (value: t_mutationWithError_errors).__typename;
                             value;
                           };
                           {__typename, message, field};
                         }: Raw.t_mutationWithError_errors
                       )
                     ),
                )
              | None => Js.Nullable.null
              };
            }
            and __typename = {
              let value = (value: t_mutationWithError).__typename;
              value;
            };
            {__typename, errors};
          }: Raw.t_mutationWithError
        );
      };
      {mutationWithError: mutationWithError};
    }: Raw.t
  );
  let makeVariables = () => ();
  let makeDefaultVariables = () => makeVariables();
  external unsafe_fromJson: Js.Json.t => Raw.t = "%identity";
  external toJson: Raw.t => Js.Json.t = "%identity";
  module Z__INTERNAL = {
    type root = t;
    type nonrec graphql_module;
    /****--- graphql-ppx module ---**

The contents of this module are automatically generated by `graphql-ppx`.
The following is simply an overview of the most important variables and types that you can access from this module.

```
module ByConfig {
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
module ByDirective = {
  module Raw = {
    type t_mutationWithError_errors_field = string;
    type t_mutationWithError_errors = {
      __typename: string,
      message: string,
      field: t_mutationWithError_errors_field,
    };
    type t_mutationWithError = {
      __typename: string,
      errors: Js.Nullable.t(array(t_mutationWithError_errors)),
    };
    type t = {mutationWithError: t_mutationWithError};
    type t_variables = unit;
  };
  /**The GraphQL query string*/
  let query = "mutation   {\nmutationWithError  {\n__typename  \nerrors  {\n__typename  \nmessage  \nfield  \n}\n\n}\n\n}\n";
  type t_mutationWithError_errors_field = [ | `FIRST | `SECOND | `THIRD];
  type t_mutationWithError_errors = {
    __typename: string,
    message: string,
    field: t_mutationWithError_errors_field,
  };
  type t_mutationWithError = {
    __typename: string,
    errors: option(array(t_mutationWithError_errors)),
  };
  type t = {mutationWithError: t_mutationWithError};
  type t_variables = unit;
  /**Parse the JSON GraphQL data to ReasonML data types*/
  let parse = (value: Raw.t): t => (
    {
      mutationWithError: {
        let value = (value: Raw.t).mutationWithError;
        (
          {
            __typename: {
              let value = (value: Raw.t_mutationWithError).__typename;
              value;
            },
            errors: {
              let value = (value: Raw.t_mutationWithError).errors;
              switch (Js.toOption(value)) {
              | Some(value) =>
                Some(
                  value
                  |> Js.Array.map((value) =>
                       (
                         {
                           __typename: {
                             let value =
                               (value: Raw.t_mutationWithError_errors).
                                 __typename;
                             value;
                           },
                           message: {
                             let value =
                               (value: Raw.t_mutationWithError_errors).message;
                             value;
                           },
                           field: {
                             let value =
                               (value: Raw.t_mutationWithError_errors).field;
                             switch (Obj.magic(value: string)) {
                             | "FIRST" => `FIRST
                             | "SECOND" => `SECOND
                             | "THIRD" => `THIRD
                             | _ => raise(Not_found)
                             };
                           },
                         }: t_mutationWithError_errors
                       )
                     ),
                )
              | None => None
              };
            },
          }: t_mutationWithError
        );
      },
    }: t
  );
  /**Serialize the ReasonML GraphQL data that was parsed using the parse function back to the original JSON compatible data */
  let serialize = (value: t): Raw.t => (
    {
      let mutationWithError = {
        let value = (value: t).mutationWithError;
        (
          {
            let errors = {
              let value = (value: t_mutationWithError).errors;
              switch (value) {
              | Some(value) =>
                Js.Nullable.return(
                  value
                  |> Js.Array.map((value) =>
                       (
                         {
                           let field = {
                             let value =
                               (value: t_mutationWithError_errors).field;
                             switch (value) {
                             | `FIRST => "FIRST"
                             | `SECOND => "SECOND"
                             | `THIRD => "THIRD"
                             };
                           }
                           and message = {
                             let value =
                               (value: t_mutationWithError_errors).message;
                             value;
                           }
                           and __typename = {
                             let value =
                               (value: t_mutationWithError_errors).__typename;
                             value;
                           };
                           {__typename, message, field};
                         }: Raw.t_mutationWithError_errors
                       )
                     ),
                )
              | None => Js.Nullable.null
              };
            }
            and __typename = {
              let value = (value: t_mutationWithError).__typename;
              value;
            };
            {__typename, errors};
          }: Raw.t_mutationWithError
        );
      };
      {mutationWithError: mutationWithError};
    }: Raw.t
  );
  let makeVariables = () => ();
  let makeDefaultVariables = () => makeVariables();
  external unsafe_fromJson: Js.Json.t => Raw.t = "%identity";
  external toJson: Raw.t => Js.Json.t = "%identity";
  module Z__INTERNAL = {
    type root = t;
    type nonrec graphql_module;
    /****--- graphql-ppx module ---**

The contents of this module are automatically generated by `graphql-ppx`.
The following is simply an overview of the most important variables and types that you can access from this module.

```
module ByDirective {
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

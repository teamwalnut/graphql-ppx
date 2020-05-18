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
module Normal' = {
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
    type t_variables = Js.Json.t;
  };
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
  type t_variables = Js.Json.t;
  let parse: Raw.t => t =
    (value) => (
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
                                 (value: Raw.t_mutationWithError_errors).
                                   message;
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
  let serialize: t => Raw.t =
    (value) => (
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
                                 (value: t_mutationWithError_errors).
                                   __typename;
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
  module Z__INTERNAL = {
    type root = t;
    type nonrec graphql_module;
    /****--- GraphQL PPX Module ---**

The contents of this module are automatically generated by `graphql-ppx`.
The following is simply an overview of the most important variables and types that you can access from this module.

```
module Normal {
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
module Normal = {
  include Normal';
  module type Type = GraphQL_PPX.Mutation;
  let self: module Type = (module Normal');
};
module ByConfig' = {
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
    type t_variables = Js.Json.t;
  };
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
  type t_variables = Js.Json.t;
  let parse: Raw.t => t =
    (value) => (
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
                                 (value: Raw.t_mutationWithError_errors).
                                   message;
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
  let serialize: t => Raw.t =
    (value) => (
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
                                 (value: t_mutationWithError_errors).
                                   __typename;
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
  module Z__INTERNAL = {
    type root = t;
    type nonrec graphql_module;
    /****--- GraphQL PPX Module ---**

The contents of this module are automatically generated by `graphql-ppx`.
The following is simply an overview of the most important variables and types that you can access from this module.

```
module ByConfig {
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
module ByConfig = {
  include ByConfig';
  module type Type = GraphQL_PPX.Mutation;
  let self: module Type = (module ByConfig');
};
module ByDirective' = {
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
    type t_variables = Js.Json.t;
  };
  let query = "mutation   {\nmutationWithError  {\n__typename  \nerrors  {\n__typename  \nmessage  \nfield @ppxOmitFutureValue \n}\n\n}\n\n}\n";
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
  type t_variables = Js.Json.t;
  let parse: Raw.t => t =
    (value) => (
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
                                 (value: Raw.t_mutationWithError_errors).
                                   message;
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
  let serialize: t => Raw.t =
    (value) => (
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
                                 (value: t_mutationWithError_errors).
                                   __typename;
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
  module Z__INTERNAL = {
    type root = t;
    type nonrec graphql_module;
    /****--- GraphQL PPX Module ---**

The contents of this module are automatically generated by `graphql-ppx`.
The following is simply an overview of the most important variables and types that you can access from this module.

```
module ByDirective {
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
module ByDirective = {
  include ByDirective';
  module type Type = GraphQL_PPX.Mutation;
  let self: module Type = (module ByDirective');
};

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
    type t_mutationWithError_value = {. "stringField": string};
    type t_mutationWithError_errors_field = string;
    type t_mutationWithError_errors = {
      .
      "field": t_mutationWithError_errors_field,
      "message": string,
    };
    type t_mutationWithError = {
      .
      "value": Js.Nullable.t(t_mutationWithError_value),
      "errors": Js.Nullable.t(array(t_mutationWithError_errors)),
    };
    type t = {. "mutationWithError": t_mutationWithError};
    type t_variables = unit;
  };
  /**The GraphQL query string*/
  let query = "mutation   {\nmutationWithError  {\nvalue  {\nstringField  \n}\n\nerrors  {\nfield  \nmessage  \n}\n\n}\n\n}\n";
  type t_mutationWithError_value = {. "stringField": string};
  type t_mutationWithError_errors_field = [
    | `FutureAddedValue(string)
    | `FIRST
    | `SECOND
    | `THIRD
  ];
  type t_mutationWithError_errors = {
    .
    "field": t_mutationWithError_errors_field,
    "message": string,
  };
  type t_mutationWithError = {
    .
    "value": option(t_mutationWithError_value),
    "errors": option(array(t_mutationWithError_errors)),
  };
  type t = {. "mutationWithError": t_mutationWithError};
  type t_variables = unit;
  /**Parse the JSON GraphQL data to ReasonML data types*/
  let parse = (value: Raw.t): t => {
    let mutationWithError = {
      let value = value##mutationWithError;
      let errors = {
        let value = value##errors;
        switch (Js.toOption(value)) {
        | Some(value) =>
          Some(
            value
            |> Js.Array.map(value =>
                 let message = {
                   let value = value##message;
                   value;
                 }
                 and field = {
                   let value = value##field;
                   switch (Obj.magic(value: string)) {
                   | "FIRST" => `FIRST
                   | "SECOND" => `SECOND
                   | "THIRD" => `THIRD
                   | other => `FutureAddedValue(other)
                   };
                 };
                 {"field": field, "message": message};
               ),
          )
        | None => None
        };
      }
      and value = {
        let value = value##value;
        switch (Js.toOption(value)) {
        | Some(value) =>
          Some(
            {
              let stringField = {
                let value = value##stringField;
                value;
              };
              {"stringField": stringField};
            },
          )
        | None => None
        };
      };
      {"value": value, "errors": errors};
    };
    {"mutationWithError": mutationWithError};
  };
  /**Serialize the ReasonML GraphQL data that was parsed using the parse function back to the original JSON compatible data */
  let serialize = (value: t): Raw.t => {
    let mutationWithError = {
      let value = value##mutationWithError;
      let errors = {
        let value = value##errors;
        switch (value) {
        | Some(value) =>
          Js.Nullable.return(
            value
            |> Js.Array.map(value =>
                 let message = {
                   let value = value##message;
                   value;
                 }
                 and field = {
                   let value = value##field;
                   switch (value) {
                   | `FIRST => "FIRST"
                   | `SECOND => "SECOND"
                   | `THIRD => "THIRD"
                   | `FutureAddedValue(other) => other
                   };
                 };
                 {"field": field, "message": message};
               ),
          )
        | None => Js.Nullable.null
        };
      }
      and value = {
        let value = value##value;
        switch (value) {
        | Some(value) =>
          Js.Nullable.return(
            {
              let stringField = {
                let value = value##stringField;
                value;
              };
              {"stringField": stringField};
            },
          )
        | None => Js.Nullable.null
        };
      };
      {"value": value, "errors": errors};
    };
    {"mutationWithError": mutationWithError};
  };
  let makeVariables = () => ();
  let makeDefaultVariables = () => makeVariables();
  let make = () => {
    "query": query,
    "variables": Js.Json.null,
    "parse": parse,
  };
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

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
    type t_variousScalars = {
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
    type t = {variousScalars: t_variousScalars};
    type t_variables = Js.Json.t;
  };
  let query = "query   {\nvariousScalars  {\nnullableString  \nstring  \nnullableInt  \nint  \nnullableFloat  \nfloat  \nnullableBoolean  \nboolean  \nnullableID  \nid  \n}\n\n}\n";
  type t_variousScalars = {
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
  type t = {variousScalars: t_variousScalars};
  type t_variables = Js.Json.t;
  let parse: Raw.t => t =
    (value) => (
      {
        variousScalars: {
          let value = (value: Raw.t).variousScalars;
          (
            {
              nullableString: {
                let value = (value: Raw.t_variousScalars).nullableString;
                switch (Js.toOption(value)) {
                | Some(value) => Some(value)
                | None => None
                };
              },
              string: {
                let value = (value: Raw.t_variousScalars).string;
                value;
              },
              nullableInt: {
                let value = (value: Raw.t_variousScalars).nullableInt;
                switch (Js.toOption(value)) {
                | Some(value) => Some(value)
                | None => None
                };
              },
              int: {
                let value = (value: Raw.t_variousScalars).int;
                value;
              },
              nullableFloat: {
                let value = (value: Raw.t_variousScalars).nullableFloat;
                switch (Js.toOption(value)) {
                | Some(value) => Some(value)
                | None => None
                };
              },
              float: {
                let value = (value: Raw.t_variousScalars).float;
                value;
              },
              nullableBoolean: {
                let value = (value: Raw.t_variousScalars).nullableBoolean;
                switch (Js.toOption(value)) {
                | Some(value) => Some(value)
                | None => None
                };
              },
              boolean: {
                let value = (value: Raw.t_variousScalars).boolean;
                value;
              },
              nullableID: {
                let value = (value: Raw.t_variousScalars).nullableID;
                switch (Js.toOption(value)) {
                | Some(value) => Some(value)
                | None => None
                };
              },
              id: {
                let value = (value: Raw.t_variousScalars).id;
                value;
              },
            }: t_variousScalars
          );
        },
      }: t
    );
  let serialize: t => Raw.t =
    (value) => (
      {
        let variousScalars = {
          let value = (value: t).variousScalars;
          (
            {
              let id = {
                let value = (value: t_variousScalars).id;
                value;
              }
              and nullableID = {
                let value = (value: t_variousScalars).nullableID;
                switch (value) {
                | Some(value) => Js.Nullable.return(value)
                | None => Js.Nullable.null
                };
              }
              and boolean = {
                let value = (value: t_variousScalars).boolean;
                value;
              }
              and nullableBoolean = {
                let value = (value: t_variousScalars).nullableBoolean;
                switch (value) {
                | Some(value) => Js.Nullable.return(value)
                | None => Js.Nullable.null
                };
              }
              and float = {
                let value = (value: t_variousScalars).float;
                value;
              }
              and nullableFloat = {
                let value = (value: t_variousScalars).nullableFloat;
                switch (value) {
                | Some(value) => Js.Nullable.return(value)
                | None => Js.Nullable.null
                };
              }
              and int = {
                let value = (value: t_variousScalars).int;
                value;
              }
              and nullableInt = {
                let value = (value: t_variousScalars).nullableInt;
                switch (value) {
                | Some(value) => Js.Nullable.return(value)
                | None => Js.Nullable.null
                };
              }
              and string = {
                let value = (value: t_variousScalars).string;
                value;
              }
              and nullableString = {
                let value = (value: t_variousScalars).nullableString;
                switch (value) {
                | Some(value) => Js.Nullable.return(value)
                | None => Js.Nullable.null
                };
              };
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
              };
            }: Raw.t_variousScalars
          );
        };
        {variousScalars: variousScalars};
      }: Raw.t
    );
  module Z__INTERNAL = {
    type root = t;
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
  module type query_type = (module type of MyQuery');
  let self: module query_type = (module MyQuery');
};

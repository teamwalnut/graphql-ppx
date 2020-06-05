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
type record = {
  f1: string,
  f2: string,
};
module MyQuery = {
  module Raw = {
    type t_first_inner_inner = {. "field": string};
    type t_first_inner = {. "inner": Js.Nullable.t(t_first_inner_inner)};
    type t_first = {. "inner": Js.Nullable.t(t_first_inner)};
    type t_second_inner_inner = {
      .
      "f1": string,
      "f2": string,
    };
    type t_second_inner = {. "inner": Js.Nullable.t(t_second_inner_inner)};
    type t_second = {. "inner": Js.Nullable.t(t_second_inner)};
    type t_let_inner_inner = {. "field": string};
    type t_let_inner = {. "inner": Js.Nullable.t(t_let_inner_inner)};
    type t_let = {. "inner": Js.Nullable.t(t_let_inner)};
    type t = {
      .
      "first": t_first,
      "second": t_second,
      "let_": t_let,
    };
    type t_variables = unit;
  };
  let query = "query   {\nfirst: nestedObject  {\ninner  {\ninner  {\nfield  \n}\n\n}\n\n}\n\nsecond: nestedObject  {\ninner  {\ninner  {\nf1: field  \nf2: field  \n}\n\n}\n\n}\n\nlet: nestedObject  {\ninner  {\ninner  {\nfield  \n}\n\n}\n\n}\n\n}\n";
  type t_first_inner_inner = {. "field": string};
  type t_first_inner = {. "inner": option(t_first_inner_inner)};
  type t_first = {. "inner": option(t_first_inner)};
  type t_second_inner_inner = {
    f1: string,
    f2: string,
  };
  type t_second_inner = {. "inner": option(t_second_inner_inner)};
  type t_second = {. "inner": option(t_second_inner)};
  type t_let_inner_inner = {. "field": string};
  type t_let_inner = {. "inner": option(t_let_inner_inner)};
  type t_let = {. "inner": option(t_let_inner)};
  type t = {
    .
    "first": t_first,
    "second": t_second,
    "let_": t_let,
  };
  type t_variables = unit;
  let parse: Raw.t => t =
    value => {
      let let_ = {
        let value = value##let_;
        let inner = {
          let value = value##inner;
          switch (Js.toOption(value)) {
          | Some(value) =>
            Some(
              {
                let inner = {
                  let value = value##inner;
                  switch (Js.toOption(value)) {
                  | Some(value) =>
                    Some(
                      {
                        let field = {
                          let value = value##field;
                          value;
                        };
                        {"field": field};
                      },
                    )
                  | None => None
                  };
                };
                {"inner": inner};
              },
            )
          | None => None
          };
        };
        {"inner": inner};
      }
      and second = {
        let value = value##second;
        let inner = {
          let value = value##inner;
          switch (Js.toOption(value)) {
          | Some(value) =>
            Some(
              {
                let inner = {
                  let value = value##inner;
                  switch (Js.toOption(value)) {
                  | Some(value) =>
                    Some(
                      {
                        let f2 = {
                          let value = value##f2;
                          value;
                        }
                        and f1 = {
                          let value = value##f1;
                          value;
                        };
                        {f1, f2};
                      }: t_second_inner_inner,
                    )
                  | None => None
                  };
                };
                {"inner": inner};
              },
            )
          | None => None
          };
        };
        {"inner": inner};
      }
      and first = {
        let value = value##first;
        let inner = {
          let value = value##inner;
          switch (Js.toOption(value)) {
          | Some(value) =>
            Some(
              {
                let inner = {
                  let value = value##inner;
                  switch (Js.toOption(value)) {
                  | Some(value) =>
                    Some(
                      {
                        let field = {
                          let value = value##field;
                          value;
                        };
                        {"field": field};
                      },
                    )
                  | None => None
                  };
                };
                {"inner": inner};
              },
            )
          | None => None
          };
        };
        {"inner": inner};
      };
      {"first": first, "second": second, "let_": let_};
    };
  let serialize: t => Raw.t =
    value => {
      let let_ = {
        let value = value##let_;
        let inner = {
          let value = value##inner;
          switch (value) {
          | Some(value) =>
            Js.Nullable.return(
              {
                let inner = {
                  let value = value##inner;
                  switch (value) {
                  | Some(value) =>
                    Js.Nullable.return(
                      {
                        let field = {
                          let value = value##field;
                          value;
                        };
                        {"field": field};
                      },
                    )
                  | None => Js.Nullable.null
                  };
                };
                {"inner": inner};
              },
            )
          | None => Js.Nullable.null
          };
        };
        {"inner": inner};
      }
      and second = {
        let value = value##second;
        let inner = {
          let value = value##inner;
          switch (value) {
          | Some(value) =>
            Js.Nullable.return(
              {
                let inner = {
                  let value = value##inner;
                  switch (value) {
                  | Some(value) =>
                    Js.Nullable.return(
                      {
                        let f2 = {
                          let value = (value: t_second_inner_inner).f2;
                          value;
                        }
                        and f1 = {
                          let value = (value: t_second_inner_inner).f1;
                          value;
                        };
                        {"f1": f1, "f2": f2};
                      },
                    )
                  | None => Js.Nullable.null
                  };
                };
                {"inner": inner};
              },
            )
          | None => Js.Nullable.null
          };
        };
        {"inner": inner};
      }
      and first = {
        let value = value##first;
        let inner = {
          let value = value##inner;
          switch (value) {
          | Some(value) =>
            Js.Nullable.return(
              {
                let inner = {
                  let value = value##inner;
                  switch (value) {
                  | Some(value) =>
                    Js.Nullable.return(
                      {
                        let field = {
                          let value = value##field;
                          value;
                        };
                        {"field": field};
                      },
                    )
                  | None => Js.Nullable.null
                  };
                };
                {"inner": inner};
              },
            )
          | None => Js.Nullable.null
          };
        };
        {"inner": inner};
      };
      {"first": first, "second": second, "let_": let_};
    };
  let makeVariables = () => ();
  let makeDefaultVariables = () => makeVariables();
  let make = () => {
    "query": query,
    "variables": Js.Json.null,
    "parse": parse,
  };
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

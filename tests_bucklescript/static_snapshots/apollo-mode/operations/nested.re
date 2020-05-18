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
module MyQuery' = {
  module Raw = {
    type t_first_inner_inner = {
      __typename: string,
      field: string,
    };
    type t_first_inner = {
      __typename: string,
      inner: Js.Nullable.t(t_first_inner_inner),
    };
    type t_first = {
      __typename: string,
      inner: Js.Nullable.t(t_first_inner),
    };
    type t_second_inner_inner = {
      __typename: string,
      f1: string,
      f2: string,
    };
    type t_second_inner = {
      __typename: string,
      inner: Js.Nullable.t(t_second_inner_inner),
    };
    type t_second = {
      __typename: string,
      inner: Js.Nullable.t(t_second_inner),
    };
    type t_let_inner_inner = {
      __typename: string,
      field: string,
    };
    type t_let_inner = {
      __typename: string,
      inner: Js.Nullable.t(t_let_inner_inner),
    };
    type t_let = {
      __typename: string,
      inner: Js.Nullable.t(t_let_inner),
    };
    type t = {
      first: t_first,
      second: t_second,
      let_: t_let,
    };
    type t_variables = Js.Json.t;
  };
  let query = "query   {\nfirst: nestedObject  {\n__typename  \ninner  {\n__typename  \ninner  {\n__typename  \nfield  \n}\n\n}\n\n}\n\nsecond: nestedObject  {\n__typename  \ninner  {\n__typename  \ninner  {\n__typename  \nf1: field  \nf2: field  \n}\n\n}\n\n}\n\nlet: nestedObject  {\n__typename  \ninner  {\n__typename  \ninner  {\n__typename  \nfield  \n}\n\n}\n\n}\n\n}\n";
  type t_first_inner_inner = {
    __typename: string,
    field: string,
  };
  type t_first_inner = {
    __typename: string,
    inner: option(t_first_inner_inner),
  };
  type t_first = {
    __typename: string,
    inner: option(t_first_inner),
  };
  type t_second_inner_inner = {
    __typename: string,
    f1: string,
    f2: string,
  };
  type t_second_inner = {
    __typename: string,
    inner: option(t_second_inner_inner),
  };
  type t_second = {
    __typename: string,
    inner: option(t_second_inner),
  };
  type t_let_inner_inner = {
    __typename: string,
    field: string,
  };
  type t_let_inner = {
    __typename: string,
    inner: option(t_let_inner_inner),
  };
  type t_let = {
    __typename: string,
    inner: option(t_let_inner),
  };
  type t = {
    first: t_first,
    second: t_second,
    let_: t_let,
  };
  type t_variables = Js.Json.t;
  let parse: Raw.t => t =
    (value) => (
      {
        first: {
          let value = (value: Raw.t).first;
          (
            {
              __typename: {
                let value = (value: Raw.t_first).__typename;
                value;
              },
              inner: {
                let value = (value: Raw.t_first).inner;
                switch (Js.toOption(value)) {
                | Some(value) =>
                  Some(
                    {
                      __typename: {
                        let value = (value: Raw.t_first_inner).__typename;
                        value;
                      },
                      inner: {
                        let value = (value: Raw.t_first_inner).inner;
                        switch (Js.toOption(value)) {
                        | Some(value) =>
                          Some(
                            {
                              __typename: {
                                let value =
                                  (value: Raw.t_first_inner_inner).__typename;
                                value;
                              },
                              field: {
                                let value =
                                  (value: Raw.t_first_inner_inner).field;
                                value;
                              },
                            }: t_first_inner_inner,
                          )
                        | None => None
                        };
                      },
                    }: t_first_inner,
                  )
                | None => None
                };
              },
            }: t_first
          );
        },
        second: {
          let value = (value: Raw.t).second;
          (
            {
              __typename: {
                let value = (value: Raw.t_second).__typename;
                value;
              },
              inner: {
                let value = (value: Raw.t_second).inner;
                switch (Js.toOption(value)) {
                | Some(value) =>
                  Some(
                    {
                      __typename: {
                        let value = (value: Raw.t_second_inner).__typename;
                        value;
                      },
                      inner: {
                        let value = (value: Raw.t_second_inner).inner;
                        switch (Js.toOption(value)) {
                        | Some(value) =>
                          Some(
                            {
                              __typename: {
                                let value =
                                  (value: Raw.t_second_inner_inner).__typename;
                                value;
                              },
                              f1: {
                                let value =
                                  (value: Raw.t_second_inner_inner).f1;
                                value;
                              },
                              f2: {
                                let value =
                                  (value: Raw.t_second_inner_inner).f2;
                                value;
                              },
                            }: t_second_inner_inner,
                          )
                        | None => None
                        };
                      },
                    }: t_second_inner,
                  )
                | None => None
                };
              },
            }: t_second
          );
        },
        let_: {
          let value = (value: Raw.t).let_;
          (
            {
              __typename: {
                let value = (value: Raw.t_let).__typename;
                value;
              },
              inner: {
                let value = (value: Raw.t_let).inner;
                switch (Js.toOption(value)) {
                | Some(value) =>
                  Some(
                    {
                      __typename: {
                        let value = (value: Raw.t_let_inner).__typename;
                        value;
                      },
                      inner: {
                        let value = (value: Raw.t_let_inner).inner;
                        switch (Js.toOption(value)) {
                        | Some(value) =>
                          Some(
                            {
                              __typename: {
                                let value =
                                  (value: Raw.t_let_inner_inner).__typename;
                                value;
                              },
                              field: {
                                let value =
                                  (value: Raw.t_let_inner_inner).field;
                                value;
                              },
                            }: t_let_inner_inner,
                          )
                        | None => None
                        };
                      },
                    }: t_let_inner,
                  )
                | None => None
                };
              },
            }: t_let
          );
        },
      }: t
    );
  let serialize: t => Raw.t =
    (value) => (
      {
        let let_ = {
          let value = (value: t).let_;
          (
            {
              let inner = {
                let value = (value: t_let).inner;
                switch (value) {
                | Some(value) =>
                  Js.Nullable.return(
                    {
                      let inner = {
                        let value = (value: t_let_inner).inner;
                        switch (value) {
                        | Some(value) =>
                          Js.Nullable.return(
                            {
                              let field = {
                                let value = (value: t_let_inner_inner).field;
                                value;
                              }
                              and __typename = {
                                let value =
                                  (value: t_let_inner_inner).__typename;
                                value;
                              };
                              {__typename, field};
                            }: Raw.t_let_inner_inner,
                          )
                        | None => Js.Nullable.null
                        };
                      }
                      and __typename = {
                        let value = (value: t_let_inner).__typename;
                        value;
                      };
                      {__typename, inner};
                    }: Raw.t_let_inner,
                  )
                | None => Js.Nullable.null
                };
              }
              and __typename = {
                let value = (value: t_let).__typename;
                value;
              };
              {__typename, inner};
            }: Raw.t_let
          );
        }
        and second = {
          let value = (value: t).second;
          (
            {
              let inner = {
                let value = (value: t_second).inner;
                switch (value) {
                | Some(value) =>
                  Js.Nullable.return(
                    {
                      let inner = {
                        let value = (value: t_second_inner).inner;
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
                              }
                              and __typename = {
                                let value =
                                  (value: t_second_inner_inner).__typename;
                                value;
                              };
                              {__typename, f1, f2};
                            }: Raw.t_second_inner_inner,
                          )
                        | None => Js.Nullable.null
                        };
                      }
                      and __typename = {
                        let value = (value: t_second_inner).__typename;
                        value;
                      };
                      {__typename, inner};
                    }: Raw.t_second_inner,
                  )
                | None => Js.Nullable.null
                };
              }
              and __typename = {
                let value = (value: t_second).__typename;
                value;
              };
              {__typename, inner};
            }: Raw.t_second
          );
        }
        and first = {
          let value = (value: t).first;
          (
            {
              let inner = {
                let value = (value: t_first).inner;
                switch (value) {
                | Some(value) =>
                  Js.Nullable.return(
                    {
                      let inner = {
                        let value = (value: t_first_inner).inner;
                        switch (value) {
                        | Some(value) =>
                          Js.Nullable.return(
                            {
                              let field = {
                                let value = (value: t_first_inner_inner).field;
                                value;
                              }
                              and __typename = {
                                let value =
                                  (value: t_first_inner_inner).__typename;
                                value;
                              };
                              {__typename, field};
                            }: Raw.t_first_inner_inner,
                          )
                        | None => Js.Nullable.null
                        };
                      }
                      and __typename = {
                        let value = (value: t_first_inner).__typename;
                        value;
                      };
                      {__typename, inner};
                    }: Raw.t_first_inner,
                  )
                | None => Js.Nullable.null
                };
              }
              and __typename = {
                let value = (value: t_first).__typename;
                value;
              };
              {__typename, inner};
            }: Raw.t_first
          );
        };
        {first, second, let_};
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
  module type Type = GraphQL_PPX.Query;
  let self: module Type = (module MyQuery');
};

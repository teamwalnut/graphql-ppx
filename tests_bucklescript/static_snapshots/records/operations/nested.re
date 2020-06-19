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
  [@ocaml.warning "-32"];
  module Raw = {
    type t_first_inner_inner = {field: string};
    type t_first_inner = {inner: Js.Nullable.t(t_first_inner_inner)};
    type t_first = {inner: Js.Nullable.t(t_first_inner)};
    type t_second_inner_inner = {
      f1: string,
      f2: string,
    };
    type t_second_inner = {inner: Js.Nullable.t(t_second_inner_inner)};
    type t_second = {inner: Js.Nullable.t(t_second_inner)};
    type t_let_inner_inner = {field: string};
    type t_let_inner = {inner: Js.Nullable.t(t_let_inner_inner)};
    type t_let = {inner: Js.Nullable.t(t_let_inner)};
    type t = {
      first: t_first,
      second: t_second,
      let_: t_let,
    };
    type t_variables = unit;
  };
  /**The GraphQL query string*/
  let query = "query   {\nfirst: nestedObject  {\ninner  {\ninner  {\nfield  \n}\n\n}\n\n}\n\nsecond: nestedObject  {\ninner  {\ninner  {\nf1: field  \nf2: field  \n}\n\n}\n\n}\n\nlet: nestedObject  {\ninner  {\ninner  {\nfield  \n}\n\n}\n\n}\n\n}\n";
  type t_first_inner_inner = {field: string};
  type t_first_inner = {inner: option(t_first_inner_inner)};
  type t_first = {inner: option(t_first_inner)};
  type t_second_inner_inner = {
    f1: string,
    f2: string,
  };
  type t_second_inner = {inner: option(t_second_inner_inner)};
  type t_second = {inner: option(t_second_inner)};
  type t_let_inner_inner = {field: string};
  type t_let_inner = {inner: option(t_let_inner_inner)};
  type t_let = {inner: option(t_let_inner)};
  type t = {
    first: t_first,
    second: t_second,
    let_: t_let,
  };
  type t_variables = unit;
  /**Parse the JSON GraphQL data to ReasonML data types*/
  let parse = (value: Raw.t): t => (
    {
      first: {
        let value = (value: Raw.t).first;
        (
          {
            inner: {
              let value = (value: Raw.t_first).inner;
              switch (Js.toOption(value)) {
              | Some(value) =>
                Some(
                  {
                    inner: {
                      let value = (value: Raw.t_first_inner).inner;
                      switch (Js.toOption(value)) {
                      | Some(value) =>
                        Some(
                          {
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
            inner: {
              let value = (value: Raw.t_second).inner;
              switch (Js.toOption(value)) {
              | Some(value) =>
                Some(
                  {
                    inner: {
                      let value = (value: Raw.t_second_inner).inner;
                      switch (Js.toOption(value)) {
                      | Some(value) =>
                        Some(
                          {
                            f1: {
                              let value = (value: Raw.t_second_inner_inner).f1;
                              value;
                            },
                            f2: {
                              let value = (value: Raw.t_second_inner_inner).f2;
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
            inner: {
              let value = (value: Raw.t_let).inner;
              switch (Js.toOption(value)) {
              | Some(value) =>
                Some(
                  {
                    inner: {
                      let value = (value: Raw.t_let_inner).inner;
                      switch (Js.toOption(value)) {
                      | Some(value) =>
                        Some(
                          {
                            field: {
                              let value = (value: Raw.t_let_inner_inner).field;
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
  /**Serialize the ReasonML GraphQL data that was parsed using the parse function back to the original JSON compatible data */
  let serialize = (value: t): Raw.t => (
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
                            };
                            {field: field};
                          }: Raw.t_let_inner_inner,
                        )
                      | None => Js.Nullable.null
                      };
                    };
                    {inner: inner};
                  }: Raw.t_let_inner,
                )
              | None => Js.Nullable.null
              };
            };
            {inner: inner};
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
                            };
                            {f1, f2};
                          }: Raw.t_second_inner_inner,
                        )
                      | None => Js.Nullable.null
                      };
                    };
                    {inner: inner};
                  }: Raw.t_second_inner,
                )
              | None => Js.Nullable.null
              };
            };
            {inner: inner};
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
                            };
                            {field: field};
                          }: Raw.t_first_inner_inner,
                        )
                      | None => Js.Nullable.null
                      };
                    };
                    {inner: inner};
                  }: Raw.t_first_inner,
                )
              | None => Js.Nullable.null
              };
            };
            {inner: inner};
          }: Raw.t_first
        );
      };
      {first, second, let_};
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

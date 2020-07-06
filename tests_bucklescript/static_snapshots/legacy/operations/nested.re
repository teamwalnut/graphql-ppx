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
  /**The GraphQL query string*/
  let query = "query   {\nfirst: nestedObject  {\ninner  {\ninner  {\nfield  \n}\n\n}\n\n}\n\nsecond: nestedObject  {\ninner  {\ninner  {\nf1: field  \nf2: field  \n}\n\n}\n\n}\n\nlet: nestedObject  {\ninner  {\ninner  {\nfield  \n}\n\n}\n\n}\n\n}\n";
  type t_variables = unit;
  /**Parse the JSON GraphQL data to ReasonML data types*/
  let parse = (value: Raw.t): t => {
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
  /**Serialize the ReasonML GraphQL data that was parsed using the parse function back to the original JSON compatible data */
  let serialize = (value: t): Raw.t => {
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
  let serializeVariables = () => ();
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

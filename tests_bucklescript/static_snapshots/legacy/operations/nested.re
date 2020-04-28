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
      f1: string,
      f2: string,
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
  let parse: Raw.t => t =
    value => {
      "first": {
        let value = value##first;
        {
          "inner": {
            let value = value##inner;
            switch (Js.toOption(value)) {
            | Some(value) =>
              Some({
                "inner": {
                  let value = value##inner;
                  switch (Js.toOption(value)) {
                  | Some(value) =>
                    Some({
                      "field": {
                        let value = value##field;
                        value;
                      },
                    })
                  | None => None
                  };
                },
              })
            | None => None
            };
          },
        };
      },
      "second": {
        let value = value##second;
        {
          "inner": {
            let value = value##inner;
            switch (Js.toOption(value)) {
            | Some(value) =>
              Some({
                "inner": {
                  let value = value##inner;
                  switch (Js.toOption(value)) {
                  | Some(value) =>
                    Some({
                      "f1": {
                        let value = value##f1;
                        value;
                      },
                      "f2": {
                        let value = value##f2;
                        value;
                      },
                    })
                  | None => None
                  };
                },
              })
            | None => None
            };
          },
        };
      },
      "let_": {
        let value = value##let_;
        {
          "inner": {
            let value = value##inner;
            switch (Js.toOption(value)) {
            | Some(value) =>
              Some({
                "inner": {
                  let value = value##inner;
                  switch (Js.toOption(value)) {
                  | Some(value) =>
                    Some({
                      "field": {
                        let value = value##field;
                        value;
                      },
                    })
                  | None => None
                  };
                },
              })
            | None => None
            };
          },
        };
      },
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
                        {

                          "field": field,
                        };
                      },
                    )
                  | None => Js.Nullable.null
                  };
                };
                {

                  "inner": inner,
                };
              },
            )
          | None => Js.Nullable.null
          };
        };
        {

          "inner": inner,
        };
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
                          let value = value##f2;

                          value;
                        }
                        and f1 = {
                          let value = value##f1;

                          value;
                        };
                        {

                          "f1": f1,

                          "f2": f2,
                        };
                      },
                    )
                  | None => Js.Nullable.null
                  };
                };
                {

                  "inner": inner,
                };
              },
            )
          | None => Js.Nullable.null
          };
        };
        {

          "inner": inner,
        };
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
                        {

                          "field": field,
                        };
                      },
                    )
                  | None => Js.Nullable.null
                  };
                };
                {

                  "inner": inner,
                };
              },
            )
          | None => Js.Nullable.null
          };
        };
        {

          "inner": inner,
        };
      };
      {

        "first": first,

        "second": second,

        "let_": let_,
      };
    };
  let make = () => {
    "query": query,
    "variables": Js.Json.null,
    "parse": parse,
  };
  let definition = (parse, query, serialize);
};

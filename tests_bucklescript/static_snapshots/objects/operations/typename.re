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
    type t = {. "first": t_first}
    and t_first = {
      .
      "__typename": string,
      "inner": Js.Nullable.t(t_first_inner),
    }
    and t_first_inner = {
      .
      "__typename": string,
      "inner": Js.Nullable.t(t_first_inner_inner),
    }
    and t_first_inner_inner = {
      .
      "__typename": string,
      "field": string,
    };
  };
  let query = "query   {\nfirst: nestedObject  {\n__typename  \ninner  {\n__typename  \ninner  {\n__typename  \nfield  \n}\n\n}\n\n}\n\n}\n";
  type t = {. "first": t_first}
  and t_first = {
    .
    "__typename": string,
    "inner": option(t_first_inner),
  }
  and t_first_inner = {
    .
    "__typename": string,
    "inner": option(t_first_inner_inner),
  }
  and t_first_inner_inner = {
    .
    "__typename": string,
    "field": string,
  };
  let parse: Raw.t => t =
    value => {

      "first": {
        let value = value##first;
        {

          "__typename": {
            let value = value##__typename;

            value;
          },

          "inner": {
            let value = value##inner;

            switch (Js.toOption(value)) {
            | Some(value) =>
              Some({

                "__typename": {
                  let value = value##__typename;

                  value;
                },

                "inner": {
                  let value = value##inner;

                  switch (Js.toOption(value)) {
                  | Some(value) =>
                    Some({

                      "__typename": {
                        let value = value##__typename;

                        value;
                      },

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
      let first = {
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
                        }
                        and __typename = {
                          let value = value##__typename;

                          value;
                        };
                        {

                          "__typename": __typename,

                          "field": field,
                        };
                      },
                    )
                  | None => Js.Nullable.null
                  };
                }
                and __typename = {
                  let value = value##__typename;

                  value;
                };
                {

                  "__typename": __typename,

                  "inner": inner,
                };
              },
            )
          | None => Js.Nullable.null
          };
        }
        and __typename = {
          let value = value##__typename;

          value;
        };
        {

          "__typename": __typename,

          "inner": inner,
        };
      };
      {

        "first": first,
      };
    };
  let definition = (parse, query, serialize);
};

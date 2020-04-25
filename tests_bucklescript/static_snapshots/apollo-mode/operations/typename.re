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
    type t = {first: t_first};
  };
  let query = "query   {\nfirst: nestedObject  {\n__typename  \ninner  {\n__typename  \ninner  {\n__typename  \nfield  \n}\n\n}\n\n}\n\n}\n";
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
  type t = {first: t_first};
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
      }: t
    );
  let serialize: t => Raw.t =
    (value) => (
      {
        let first = {
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
                              {

                                __typename,

                                field,
                              };
                            }: Raw.t_first_inner_inner,
                          )
                        | None => Js.Nullable.null
                        };
                      }
                      and __typename = {
                        let value = (value: t_first_inner).__typename;

                        value;
                      };
                      {

                        __typename,

                        inner,
                      };
                    }: Raw.t_first_inner,
                  )
                | None => Js.Nullable.null
                };
              }
              and __typename = {
                let value = (value: t_first).__typename;

                value;
              };
              {

                __typename,

                inner,
              };
            }: Raw.t_first
          );
        };
        {

          first: first,
        };
      }: Raw.t
    );
  let definition = (parse, query, serialize);
};

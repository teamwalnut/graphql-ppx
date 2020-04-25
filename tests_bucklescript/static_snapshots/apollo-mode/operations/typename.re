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
    type t = {
      __typename: string,
      first: t_first,
    }
    and t_first = {
      __typename: string,
      __typename: string,
      inner: Js.Nullable.t(t_first_inner),
    }
    and t_first_inner = {
      __typename: string,
      __typename: string,
      inner: Js.Nullable.t(t_first_inner_inner),
    }
    and t_first_inner_inner = {
      __typename: string,
      __typename: string,
      field: string,
    };
  };
  let query = "query   {\n__typename  \nfirst: nestedObject  {\n__typename  \n__typename  \ninner  {\n__typename  \n__typename  \ninner  {\n__typename  \n__typename  \nfield  \n}\n\n}\n\n}\n\n}\n";
  type t = {
    __typename: string,
    first: t_first,
  }
  and t_first = {
    __typename: string,
    __typename: string,
    inner: option(t_first_inner),
  }
  and t_first_inner = {
    __typename: string,
    __typename: string,
    inner: option(t_first_inner_inner),
  }
  and t_first_inner_inner = {
    __typename: string,
    __typename: string,
    field: string,
  };
  let parse: Raw.t => t =
    (value) => (
      {

        __typename: {
          let value = (value: Raw.t).__typename;

          value;
        },

        first: {
          let value = (value: Raw.t).first;
          (
            {

              __typename: {
                let value = (value: Raw.t_first).__typename;

                value;
              },

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
                              }
                              and __typename = {
                                let value =
                                  (value: t_first_inner_inner).__typename;

                                value;
                              };
                              {

                                __typename,

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
                      }
                      and __typename = {
                        let value = (value: t_first_inner).__typename;

                        value;
                      };
                      {

                        __typename,

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
              }
              and __typename = {
                let value = (value: t_first).__typename;

                value;
              };
              {

                __typename,

                __typename,

                inner,
              };
            }: Raw.t_first
          );
        }
        and __typename = {
          let value = (value: t).__typename;

          value;
        };
        {

          __typename,

          first,
        };
      }: Raw.t
    );
  let makeVar = (~f, ()) => f(Js.Json.null);
  let definition = (parse, query, makeVar);
};

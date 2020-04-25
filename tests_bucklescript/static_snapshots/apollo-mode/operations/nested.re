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
    type t = {
      first: t_first,
      second: t_second,
      let_: t_let,
    }
    and t_let = {
      __typename: string,
      inner: Js.Nullable.t(t_let_inner),
    }
    and t_let_inner = {
      __typename: string,
      inner: Js.Nullable.t(t_let_inner_inner),
    }
    and t_let_inner_inner = {
      __typename: string,
      field: string,
    }
    and t_second = {
      __typename: string,
      inner: Js.Nullable.t(t_second_inner),
    }
    and t_second_inner = {
      __typename: string,
      inner: Js.Nullable.t(t_second_inner_inner),
    }
    and t_second_inner_inner = {
      __typename: string,
      f1: string,
      f2: string,
    }
    and t_first = {
      __typename: string,
      inner: Js.Nullable.t(t_first_inner),
    }
    and t_first_inner = {
      __typename: string,
      inner: Js.Nullable.t(t_first_inner_inner),
    }
    and t_first_inner_inner = {
      __typename: string,
      field: string,
    };
  };
  let query = "query   {\nfirst: nestedObject  {\n__typename  \ninner  {\n__typename  \ninner  {\n__typename  \nfield  \n}\n\n}\n\n}\n\nsecond: nestedObject  {\n__typename  \ninner  {\n__typename  \ninner  {\n__typename  \nf1: field  \nf2: field  \n}\n\n}\n\n}\n\nlet: nestedObject  {\n__typename  \ninner  {\n__typename  \ninner  {\n__typename  \nfield  \n}\n\n}\n\n}\n\n}\n";
  type t = {
    first: t_first,
    second: t_second,
    let_: t_let,
  }
  and t_let = {
    __typename: string,
    inner: option(t_let_inner),
  }
  and t_let_inner = {
    __typename: string,
    inner: option(t_let_inner_inner),
  }
  and t_let_inner_inner = {
    __typename: string,
    field: string,
  }
  and t_second = {
    __typename: string,
    inner: option(t_second_inner),
  }
  and t_second_inner = {
    __typename: string,
    inner: option(t_second_inner_inner),
  }
  and t_second_inner_inner = {
    __typename: string,
    f1: string,
    f2: string,
  }
  and t_first = {
    __typename: string,
    inner: option(t_first_inner),
  }
  and t_first_inner = {
    __typename: string,
    inner: option(t_first_inner_inner),
  }
  and t_first_inner_inner = {
    __typename: string,
    field: string,
  };
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
                              {

                                __typename,

                                field,
                              };
                            }: Raw.t_let_inner_inner,
                          )
                        | None => Js.Nullable.null
                        };
                      }
                      and __typename = {
                        let value = (value: t_let_inner).__typename;

                        value;
                      };
                      {

                        __typename,

                        inner,
                      };
                    }: Raw.t_let_inner,
                  )
                | None => Js.Nullable.null
                };
              }
              and __typename = {
                let value = (value: t_let).__typename;

                value;
              };
              {

                __typename,

                inner,
              };
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
                              {

                                __typename,

                                f1,

                                f2,
                              };
                            }: Raw.t_second_inner_inner,
                          )
                        | None => Js.Nullable.null
                        };
                      }
                      and __typename = {
                        let value = (value: t_second_inner).__typename;

                        value;
                      };
                      {

                        __typename,

                        inner,
                      };
                    }: Raw.t_second_inner,
                  )
                | None => Js.Nullable.null
                };
              }
              and __typename = {
                let value = (value: t_second).__typename;

                value;
              };
              {

                __typename,

                inner,
              };
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

          first,

          second,

          let_,
        };
      }: Raw.t
    );
  let definition = (parse, query, serialize);
};

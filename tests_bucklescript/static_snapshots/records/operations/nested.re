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
    and t_let = {inner: Js.Nullable.t(t_let_inner)}
    and t_let_inner = {inner: Js.Nullable.t(t_let_inner_inner)}
    and t_let_inner_inner = {field: string}
    and t_second = {inner: Js.Nullable.t(t_second_inner)}
    and t_second_inner = {inner: Js.Nullable.t(t_second_inner_inner)}
    and t_second_inner_inner = {
      f1: string,
      f2: string,
    }
    and t_first = {inner: Js.Nullable.t(t_first_inner)}
    and t_first_inner = {inner: Js.Nullable.t(t_first_inner_inner)}
    and t_first_inner_inner = {field: string};
  };
  let query = "query   {\nfirst: nestedObject  {\ninner  {\ninner  {\nfield  \n}\n\n}\n\n}\n\nsecond: nestedObject  {\ninner  {\ninner  {\nf1: field  \nf2: field  \n}\n\n}\n\n}\n\nlet: nestedObject  {\ninner  {\ninner  {\nfield  \n}\n\n}\n\n}\n\n}\n";
  type t = {
    first: t_first,
    second: t_second,
    let_: t_let,
  }
  and t_let = {inner: option(t_let_inner)}
  and t_let_inner = {inner: option(t_let_inner_inner)}
  and t_let_inner_inner = {field: string}
  and t_second = {inner: option(t_second_inner)}
  and t_second_inner = {inner: option(t_second_inner_inner)}
  and t_second_inner_inner = {
    f1: string,
    f2: string,
  }
  and t_first = {inner: option(t_first_inner)}
  and t_first_inner = {inner: option(t_first_inner_inner)}
  and t_first_inner_inner = {field: string};
  let parse: Js.Json.t => t =
    (value) => (
      {

        first: {
          let value = Js.Dict.unsafeGet(Obj.magic(value), "first");
          (
            {

              inner: {
                let value = Js.Dict.unsafeGet(Obj.magic(value), "inner");

                switch (Js.toOption(Obj.magic(value): Js.Nullable.t('a))) {
                | Some(_) =>
                  Some(
                    {

                      inner: {
                        let value =
                          Js.Dict.unsafeGet(Obj.magic(value), "inner");

                        switch (
                          Js.toOption(Obj.magic(value): Js.Nullable.t('a))
                        ) {
                        | Some(_) =>
                          Some(
                            {

                              field: {
                                let value =
                                  Js.Dict.unsafeGet(
                                    Obj.magic(value),
                                    "field",
                                  );

                                (Obj.magic(value): string);
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
          let value = Js.Dict.unsafeGet(Obj.magic(value), "second");
          (
            {

              inner: {
                let value = Js.Dict.unsafeGet(Obj.magic(value), "inner");

                switch (Js.toOption(Obj.magic(value): Js.Nullable.t('a))) {
                | Some(_) =>
                  Some(
                    {

                      inner: {
                        let value =
                          Js.Dict.unsafeGet(Obj.magic(value), "inner");

                        switch (
                          Js.toOption(Obj.magic(value): Js.Nullable.t('a))
                        ) {
                        | Some(_) =>
                          Some(
                            {

                              f1: {
                                let value =
                                  Js.Dict.unsafeGet(Obj.magic(value), "f1");

                                (Obj.magic(value): string);
                              },

                              f2: {
                                let value =
                                  Js.Dict.unsafeGet(Obj.magic(value), "f2");

                                (Obj.magic(value): string);
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
          let value = Js.Dict.unsafeGet(Obj.magic(value), "let");
          (
            {

              inner: {
                let value = Js.Dict.unsafeGet(Obj.magic(value), "inner");

                switch (Js.toOption(Obj.magic(value): Js.Nullable.t('a))) {
                | Some(_) =>
                  Some(
                    {

                      inner: {
                        let value =
                          Js.Dict.unsafeGet(Obj.magic(value), "inner");

                        switch (
                          Js.toOption(Obj.magic(value): Js.Nullable.t('a))
                        ) {
                        | Some(_) =>
                          Some(
                            {

                              field: {
                                let value =
                                  Js.Dict.unsafeGet(
                                    Obj.magic(value),
                                    "field",
                                  );

                                (Obj.magic(value): string);
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
  let makeVar = (~f, ()) => f(Js.Json.null);
  let definition = (parse, query, makeVar);
};

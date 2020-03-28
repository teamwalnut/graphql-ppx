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
  let query = "query   {\nfirst: nestedObject  {\n__typename  \ninner  {\n__typename  \ninner  {\n__typename  \nfield  \n}\n\n}\n\n}\n\n}\n";
  type raw_t;
  type t = {first: t_first}
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
  let parse: Js.Json.t => t =
    (value) => (
      {

        first: {
          let value = Js.Dict.unsafeGet(Obj.magic(value), "first");
          (
            {

              __typename: {
                let value =
                  Js.Dict.unsafeGet(Obj.magic(value), "__typename");

                (Obj.magic(value): string);
              },

              inner: {
                let value = Js.Dict.unsafeGet(Obj.magic(value), "inner");

                switch (Js.toOption(Obj.magic(value): Js.Nullable.t('a))) {
                | Some(_) =>
                  Some(
                    {

                      __typename: {
                        let value =
                          Js.Dict.unsafeGet(Obj.magic(value), "__typename");

                        (Obj.magic(value): string);
                      },

                      inner: {
                        let value =
                          Js.Dict.unsafeGet(Obj.magic(value), "inner");

                        switch (
                          Js.toOption(Obj.magic(value): Js.Nullable.t('a))
                        ) {
                        | Some(_) =>
                          Some(
                            {

                              __typename: {
                                let value =
                                  Js.Dict.unsafeGet(
                                    Obj.magic(value),
                                    "__typename",
                                  );

                                (Obj.magic(value): string);
                              },

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
      }: t
    );
  let makeVar = (~f, ()) => f(Js.Json.null);
  let definition = (parse, query, makeVar);
};

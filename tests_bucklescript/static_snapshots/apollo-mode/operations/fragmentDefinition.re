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
module GraphQL_PPX = {
  let%private clone: Js.Dict.t('a) => Js.Dict.t('a) =
    a => Obj.magic(Js.Obj.assign(Obj.magic(Js.Obj.empty()), Obj.magic(a)));

  let rec deepMerge = (json1: Js.Json.t, json2: Js.Json.t) =>
    switch (
      (
        Obj.magic(json1) == Js.null,
        Js_array2.isArray(json1),
        Js.typeof(json1) == "object",
      ),
      (
        Obj.magic(json2) == Js.null,
        Js_array2.isArray(json2),
        Js.typeof(json2) == "object",
      ),
    ) {
    | ((_, true, _), (_, true, _)) => (
        Obj.magic(
          Js.Array.mapi(
            (el1, idx) => {
              let el2 = Js.Array.unsafe_get(Obj.magic(json2), idx);

              Js.typeof(el2) == "object" ? deepMerge(el1, el2) : el2;
            },
            Obj.magic(json1),
          ),
        ): Js.Json.t
      )

    | ((false, false, true), (false, false, true)) =>
      let obj1 = clone(Obj.magic(json1));
      let obj2 = Obj.magic(json2);
      Js.Dict.keys(obj2)
      |> Js.Array.forEach(key =>
           let existingVal: Js.Json.t = Js.Dict.unsafeGet(obj1, key);
           let newVal: Js.Json.t = Js.Dict.unsafeGet(obj1, key);
           Js.Dict.set(
             obj1,
             key,
             Js.typeof(existingVal) != "object"
               ? newVal : Obj.magic(deepMerge(existingVal, newVal)),
           );
         );
      Obj.magic(obj1);

    | ((_, _, _), (_, _, _)) => json2
    };
};

module Fragments = {
  module ListFragment = {
    let query = "fragment ListFragment on Lists   {\n__typename  \nnullableOfNullable  \nnullableOfNonNullable  \n}\n";
    module Raw = {
      type t = {
        __typename: string,
        nullableOfNullable: Js.Nullable.t(array(Js.Nullable.t(string))),
        nullableOfNonNullable: Js.Nullable.t(array(string)),
      };
      type t_Lists = t;
    };
    type t = {
      __typename: string,
      nullableOfNullable: option(array(option(string))),
      nullableOfNonNullable: option(array(string)),
    };
    type t_Lists = t;

    let parse = (value: Raw.t): t => {

      __typename: {
        let value = (value: Raw.t).__typename;

        value;
      },

      nullableOfNullable: {
        let value = (value: Raw.t).nullableOfNullable;

        switch (Js.toOption(value)) {
        | Some(value) =>
          Some(
            value
            |> Js.Array.map(value =>
                 switch (Js.toOption(value)) {
                 | Some(value) => Some(value)
                 | None => None
                 }
               ),
          )
        | None => None
        };
      },

      nullableOfNonNullable: {
        let value = (value: Raw.t).nullableOfNonNullable;

        switch (Js.toOption(value)) {
        | Some(value) => Some(value |> Js.Array.map(value => value))
        | None => None
        };
      },
    };
    let serialize: t => Raw.t =
      (value) => (
        {
          let nullableOfNonNullable = {
            let value = (value: t).nullableOfNonNullable;

            switch (value) {
            | Some(value) =>
              Js.Nullable.return(value |> Js.Array.map(value => value))
            | None => Js.Nullable.null
            };
          }
          and nullableOfNullable = {
            let value = (value: t).nullableOfNullable;

            switch (value) {
            | Some(value) =>
              Js.Nullable.return(
                value
                |> Js.Array.map(value =>
                     switch (value) {
                     | Some(value) => Js.Nullable.return(value)
                     | None => Js.Nullable.null
                     }
                   ),
              )
            | None => Js.Nullable.null
            };
          }
          and __typename = {
            let value = (value: t).__typename;

            value;
          };
          {

            __typename,

            nullableOfNullable,

            nullableOfNonNullable,
          };
        }: Raw.t
      );
    let name = "ListFragment";
  };
  module Another = {
    let query = "fragment Another on Lists   {\n__typename  \nnullableOfNonNullable  \n}\n";
    module Raw = {
      type t = {
        __typename: string,
        nullableOfNonNullable: Js.Nullable.t(array(string)),
      };
      type t_Lists = t;
    };
    type t = {
      __typename: string,
      nullableOfNonNullable: option(array(string)),
    };
    type t_Lists = t;

    let parse = (value: Raw.t): t => {

      __typename: {
        let value = (value: Raw.t).__typename;

        value;
      },

      nullableOfNonNullable: {
        let value = (value: Raw.t).nullableOfNonNullable;

        switch (Js.toOption(value)) {
        | Some(value) => Some(value |> Js.Array.map(value => value))
        | None => None
        };
      },
    };
    let serialize: t => Raw.t =
      (value) => (
        {
          let nullableOfNonNullable = {
            let value = (value: t).nullableOfNonNullable;

            switch (value) {
            | Some(value) =>
              Js.Nullable.return(value |> Js.Array.map(value => value))
            | None => Js.Nullable.null
            };
          }
          and __typename = {
            let value = (value: t).__typename;

            value;
          };
          {

            __typename,

            nullableOfNonNullable,
          };
        }: Raw.t
      );
    let name = "Another";
  };
};

module MyQuery = {
  module Raw = {
    type t = {
      l1: Fragments.ListFragment.Raw.t,
      l2: t_l2,
      l3: t_l3,
      l4: t_l4,
    }
    and t_l4
    and t_l3
    and t_l2;
  };
  let query =
    (
      (
        (
          (
            (
              (
                (
                  (
                    (
                      (
                        (
                          (
                            "query   {\nl1: lists  {\n..."
                            ++ Fragments.ListFragment.name
                          )
                          ++ "   \n}\n\nl2: lists  {\n__typename  \n..."
                        )
                        ++ Fragments.ListFragment.name
                      )
                      ++ "   \n..."
                    )
                    ++ Fragments.ListFragment.name
                  )
                  ++ "   \n}\n\nl3: lists  {\n__typename  \nnullableOfNullable  \n..."
                )
                ++ Fragments.ListFragment.name
              )
              ++ "   \n..."
            )
            ++ Fragments.ListFragment.name
          )
          ++ "   \n}\n\nl4: lists  {\n__typename  \nnullableOfNullable  \n..."
        )
        ++ Fragments.ListFragment.name
      )
      ++ "   \n}\n\n}\n"
    )
    ++ Fragments.ListFragment.query;
  type t = {
    l1: Fragments.ListFragment.t,
    l2: t_l2,
    l3: t_l3,
    l4: t_l4,
  }
  and t_l4 = {
    __typename: string,
    nullableOfNullable: option(array(option(string))),
    listFragment: Fragments.ListFragment.t_Lists,
  }
  and t_l3 = {
    __typename: string,
    nullableOfNullable: option(array(option(string))),
    frag1: Fragments.ListFragment.t_Lists,
    frag2: Fragments.ListFragment.t_Lists,
  }
  and t_l2 = {
    __typename: string,
    frag1: Fragments.ListFragment.t_Lists,
    frag2: Fragments.ListFragment.t_Lists,
  };
  let parse: Raw.t => t =
    (value) => (
      {

        l1: {
          let value = (value: Raw.t).l1;

          Fragments.ListFragment.parse(value);
        },

        l2: {
          let value = (value: Raw.t).l2;
          (
            {

              __typename: {
                let value =
                  Obj.magic(
                    Js.Dict.unsafeGet(Obj.magic(value), "__typename"),
                  );

                value;
              },

              frag1: {
                let value: Fragments.ListFragment.Raw.t = Obj.magic(value);

                Fragments.ListFragment.parse(value);
              },

              frag2: {
                let value: Fragments.ListFragment.Raw.t = Obj.magic(value);

                Fragments.ListFragment.parse(value);
              },
            }: t_l2
          );
        },

        l3: {
          let value = (value: Raw.t).l3;
          (
            {

              __typename: {
                let value =
                  Obj.magic(
                    Js.Dict.unsafeGet(Obj.magic(value), "__typename"),
                  );

                value;
              },

              nullableOfNullable: {
                let value =
                  Obj.magic(
                    Js.Dict.unsafeGet(
                      Obj.magic(value),
                      "nullableOfNullable",
                    ),
                  );

                switch (Js.toOption(value)) {
                | Some(value) =>
                  Some(
                    value
                    |> Js.Array.map(value =>
                         switch (Js.toOption(value)) {
                         | Some(value) => Some(value)
                         | None => None
                         }
                       ),
                  )
                | None => None
                };
              },

              frag1: {
                let value: Fragments.ListFragment.Raw.t = Obj.magic(value);

                Fragments.ListFragment.parse(value);
              },

              frag2: {
                let value: Fragments.ListFragment.Raw.t = Obj.magic(value);

                Fragments.ListFragment.parse(value);
              },
            }: t_l3
          );
        },

        l4: {
          let value = (value: Raw.t).l4;
          (
            {

              __typename: {
                let value =
                  Obj.magic(
                    Js.Dict.unsafeGet(Obj.magic(value), "__typename"),
                  );

                value;
              },

              nullableOfNullable: {
                let value =
                  Obj.magic(
                    Js.Dict.unsafeGet(
                      Obj.magic(value),
                      "nullableOfNullable",
                    ),
                  );

                switch (Js.toOption(value)) {
                | Some(value) =>
                  Some(
                    value
                    |> Js.Array.map(value =>
                         switch (Js.toOption(value)) {
                         | Some(value) => Some(value)
                         | None => None
                         }
                       ),
                  )
                | None => None
                };
              },

              listFragment: {
                let value: Fragments.ListFragment.Raw.t = Obj.magic(value);

                Fragments.ListFragment.parse(value);
              },
            }: t_l4
          );
        },
      }: t
    );
  let serialize: t => Raw.t =
    (value) => (
      {
        let l4 = {
          let value = (value: t).l4;
          (
            Obj.magic(
              Js.Array.reduce(
                GraphQL_PPX.deepMerge,
                Obj.magic(
                  {
                    let nullableOfNullable = {
                      let value = (value: t_l4).nullableOfNullable;

                      switch (value) {
                      | Some(value) =>
                        Js.Nullable.return(
                          value
                          |> Js.Array.map(value =>
                               switch (value) {
                               | Some(value) => Js.Nullable.return(value)
                               | None => Js.Nullable.null
                               }
                             ),
                        )
                      | None => Js.Nullable.null
                      };
                    }
                    and __typename = {
                      let value = (value: t_l4).__typename;

                      value;
                    };
                    {

                      "__typename": __typename,

                      "nullableOfNullable": nullableOfNullable,
                    };
                  },
                ): Js.Json.t,
                [|
                  (
                    Obj.magic(
                      Fragments.ListFragment.serialize(
                        (value: t_l4).listFragment,
                      ),
                    ): Js.Json.t
                  ),
                |],
              ),
            ): Raw.t_l4
          );
        }
        and l3 = {
          let value = (value: t).l3;
          (
            Obj.magic(
              Js.Array.reduce(
                GraphQL_PPX.deepMerge,
                Obj.magic(
                  {
                    let nullableOfNullable = {
                      let value = (value: t_l3).nullableOfNullable;

                      switch (value) {
                      | Some(value) =>
                        Js.Nullable.return(
                          value
                          |> Js.Array.map(value =>
                               switch (value) {
                               | Some(value) => Js.Nullable.return(value)
                               | None => Js.Nullable.null
                               }
                             ),
                        )
                      | None => Js.Nullable.null
                      };
                    }
                    and __typename = {
                      let value = (value: t_l3).__typename;

                      value;
                    };
                    {

                      "__typename": __typename,

                      "nullableOfNullable": nullableOfNullable,
                    };
                  },
                ): Js.Json.t,
                [|
                  (
                    Obj.magic(
                      Fragments.ListFragment.serialize((value: t_l3).frag1),
                    ): Js.Json.t
                  ),
                  (
                    Obj.magic(
                      Fragments.ListFragment.serialize((value: t_l3).frag2),
                    ): Js.Json.t
                  ),
                |],
              ),
            ): Raw.t_l3
          );
        }
        and l2 = {
          let value = (value: t).l2;
          (
            Obj.magic(
              Js.Array.reduce(
                GraphQL_PPX.deepMerge,
                Obj.magic(
                  {
                    let __typename = {
                      let value = (value: t_l2).__typename;

                      value;
                    };
                    {

                      "__typename": __typename,
                    };
                  },
                ): Js.Json.t,
                [|
                  (
                    Obj.magic(
                      Fragments.ListFragment.serialize((value: t_l2).frag1),
                    ): Js.Json.t
                  ),
                  (
                    Obj.magic(
                      Fragments.ListFragment.serialize((value: t_l2).frag2),
                    ): Js.Json.t
                  ),
                |],
              ),
            ): Raw.t_l2
          );
        }
        and l1 = {
          let value = (value: t).l1;
          Fragments.ListFragment.serialize(value);
        };
        {

          l1,

          l2,

          l3,

          l4,
        };
      }: Raw.t
    );
  let definition = (parse, query, serialize);
};

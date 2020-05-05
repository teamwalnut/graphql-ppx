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

module Fragments = {
  module ListFragment = {
    type graphql;
    /**```
Lists {
  nullableOfNullable: [String]
  nullableOfNonNullable: [String!]
  nonNullableOfNullable: [String]!
  nonNullableOfNonNullable: [String!]!
}
```*/
    let _: graphql = Obj.magic(0);
    let query = "fragment ListFragment on Lists   {\n__typename  \nnullableOfNullable  \nnullableOfNonNullable  \n}\n";
    module Raw = {
      type t = {
        __typename: string,
        nullableOfNullable: Js.Nullable.t(array(Js.Nullable.t(string))),
        nullableOfNonNullable: Js.Nullable.t(array(string)),
      };
      type nonrec t_Lists = t;
    };
    type t = {
      __typename: string,
      nullableOfNullable: option(array(option(string))),
      nullableOfNonNullable: option(array(string)),
    };
    type nonrec t_Lists = t;
    type fragment = t;
    let parse = (value: Raw.t): t => {
      let nullableOfNonNullable = {
        let value = (value: Raw.t).nullableOfNonNullable;
        switch (Js.toOption(value)) {
        | Some(value) => Some(value |> Js.Array.map(value => value))
        | None => None
        };
      }
      and nullableOfNullable = {
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
      }
      and __typename = {
        let value = (value: Raw.t).__typename;
        value;
      };
      {__typename, nullableOfNullable, nullableOfNonNullable};
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
          {__typename, nullableOfNullable, nullableOfNonNullable};
        }: Raw.t
      );
    let name = "ListFragment";
  };
  module Another = {
    type graphql;
    /**```
Lists {
  nullableOfNullable: [String]
  nullableOfNonNullable: [String!]
  nonNullableOfNullable: [String]!
  nonNullableOfNonNullable: [String!]!
}
```*/
    let _: graphql = Obj.magic(0);
    let query = "fragment Another on Lists   {\n__typename  \nnullableOfNonNullable  \n}\n";
    module Raw = {
      type t = {
        __typename: string,
        nullableOfNonNullable: Js.Nullable.t(array(string)),
      };
      type nonrec t_Lists = t;
    };
    type t = {
      __typename: string,
      nullableOfNonNullable: option(array(string)),
    };
    type nonrec t_Lists = t;
    type fragment = t;
    let parse = (value: Raw.t): t => {
      let nullableOfNonNullable = {
        let value = (value: Raw.t).nullableOfNonNullable;
        switch (Js.toOption(value)) {
        | Some(value) => Some(value |> Js.Array.map(value => value))
        | None => None
        };
      }
      and __typename = {
        let value = (value: Raw.t).__typename;
        value;
      };
      {__typename, nullableOfNonNullable};
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
          {__typename, nullableOfNonNullable};
        }: Raw.t
      );
    let name = "Another";
  };
};

module MyQuery = {
  module Raw = {
    type t_l2;
    type t_l3;
    type t_l4;
    type t = {
      l1: Fragments.ListFragment.Raw.t,
      l2: t_l2,
      l3: t_l3,
      l4: t_l4,
    };
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
  type t_l2 = {
    __typename: string,
    frag1: Fragments.ListFragment.t_Lists,
    frag2: Fragments.ListFragment.t_Lists,
  };
  type t_l3 = {
    __typename: string,
    nullableOfNullable: option(array(option(string))),
    frag1: Fragments.ListFragment.t_Lists,
    frag2: Fragments.ListFragment.t_Lists,
  };
  type t_l4 = {
    __typename: string,
    nullableOfNullable: option(array(option(string))),
    listFragment: Fragments.ListFragment.t_Lists,
  };
  type t = {
    l1: Fragments.ListFragment.t,
    l2: t_l2,
    l3: t_l3,
    l4: t_l4,
  };
  type operation = t;
  let parse: Raw.t => t =
    (value) => (
      {
        let l4 = {
          let value = (value: Raw.t).l4;
          (
            {
              let listFragment = {
                let value: Fragments.ListFragment.Raw.t = Obj.magic(value);
                Fragments.ListFragment.parse(value);
              }
              and nullableOfNullable = {
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
              }
              and __typename = {
                let value =
                  Obj.magic(
                    Js.Dict.unsafeGet(Obj.magic(value), "__typename"),
                  );
                value;
              };
              {__typename, nullableOfNullable, listFragment};
            }: t_l4
          );
        }
        and l3 = {
          let value = (value: Raw.t).l3;
          (
            {
              let frag2 = {
                let value: Fragments.ListFragment.Raw.t = Obj.magic(value);
                Fragments.ListFragment.parse(value);
              }
              and frag1 = {
                let value: Fragments.ListFragment.Raw.t = Obj.magic(value);
                Fragments.ListFragment.parse(value);
              }
              and nullableOfNullable = {
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
              }
              and __typename = {
                let value =
                  Obj.magic(
                    Js.Dict.unsafeGet(Obj.magic(value), "__typename"),
                  );
                value;
              };
              {__typename, nullableOfNullable, frag1, frag2};
            }: t_l3
          );
        }
        and l2 = {
          let value = (value: Raw.t).l2;
          (
            {
              let frag2 = {
                let value: Fragments.ListFragment.Raw.t = Obj.magic(value);
                Fragments.ListFragment.parse(value);
              }
              and frag1 = {
                let value: Fragments.ListFragment.Raw.t = Obj.magic(value);
                Fragments.ListFragment.parse(value);
              }
              and __typename = {
                let value =
                  Obj.magic(
                    Js.Dict.unsafeGet(Obj.magic(value), "__typename"),
                  );
                value;
              };
              {__typename, frag1, frag2};
            }: t_l2
          );
        }
        and l1 = {
          let value = (value: Raw.t).l1;
          Fragments.ListFragment.parse(value);
        };
        {l1, l2, l3, l4};
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
                    {"__typename": __typename};
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
        {l1, l2, l3, l4};
      }: Raw.t
    );
  let definition = (parse, query, serialize);
};

module MyQuery2 = {
  module Raw = {
    type t = {lists: Fragments.ListFragment.Raw.t};
  };
  let query =
    (
      ("query   {\nlists  {\n..." ++ Fragments.ListFragment.name)
      ++ "   \n}\n\n}\n"
    )
    ++ Fragments.ListFragment.query;
  type t = {lists: Fragments.ListFragment.t};
  type operation = t;
  let parse: Raw.t => t =
    (value) => (
      {
        let lists = {
          let value = (value: Raw.t).lists;
          Fragments.ListFragment.parse(value);
        };
        {lists: lists};
      }: t
    );
  let serialize: t => Raw.t =
    (value) => (
      {
        let lists = {
          let value = (value: t).lists;
          Fragments.ListFragment.serialize(value);
        };
        {lists: lists};
      }: Raw.t
    );
  let definition = (parse, query, serialize);
};

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
    let query = "fragment ListFragment on Lists   {\nnullableOfNullable  \nnullableOfNonNullable  \n}\n";
    module Raw = {
      type t = {
        nullableOfNullable: Js.Nullable.t(array(Js.Nullable.t(string))),
        nullableOfNonNullable: Js.Nullable.t(array(string)),
      };
      type t_Lists = t;
    };
    type t = {
      nullableOfNullable: option(array(option(string))),
      nullableOfNonNullable: option(array(string)),
    };
    type t_Lists = t;

    let parse = (value: Raw.t): t => {

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

          nullableOfNullable: {
            let value = (value: t).nullableOfNullable;

            switch (value) {
            | Some(value) =>
              Js.Nullable.return(
                generate_serializer(config, path, definition, inner),
              )
            | None => Js.Nullable.null
            };
          },

          nullableOfNonNullable: {
            let value = (value: t).nullableOfNonNullable;

            switch (value) {
            | Some(value) =>
              Js.Nullable.return(
                generate_serializer(config, path, definition, inner),
              )
            | None => Js.Nullable.null
            };
          },
        }: Raw.tt
      );
    let name = "ListFragment";
  };
  module Another = {
    let query = "fragment Another on Lists   {\nnullableOfNonNullable  \n}\n";
    module Raw = {
      type t = {nullableOfNonNullable: Js.Nullable.t(array(string))};
      type t_Lists = t;
    };
    type t = {nullableOfNonNullable: option(array(string))};
    type t_Lists = t;

    let parse = (value: Raw.t): t => {

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

          nullableOfNonNullable: {
            let value = (value: t).nullableOfNonNullable;

            switch (value) {
            | Some(value) =>
              Js.Nullable.return(
                generate_serializer(config, path, definition, inner),
              )
            | None => Js.Nullable.null
            };
          },
        }: Raw.tt
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
                          ++ "   \n}\n\nl2: lists  {\n..."
                        )
                        ++ Fragments.ListFragment.name
                      )
                      ++ "   \n..."
                    )
                    ++ Fragments.ListFragment.name
                  )
                  ++ "   \n}\n\nl3: lists  {\nnullableOfNullable  \n..."
                )
                ++ Fragments.ListFragment.name
              )
              ++ "   \n..."
            )
            ++ Fragments.ListFragment.name
          )
          ++ "   \n}\n\nl4: lists  {\nnullableOfNullable  \n..."
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
    nullableOfNullable: option(array(option(string))),
    listFragment: Fragments.ListFragment.t_Lists,
  }
  and t_l3 = {
    nullableOfNullable: option(array(option(string))),
    frag1: Fragments.ListFragment.t_Lists,
    frag2: Fragments.ListFragment.t_Lists,
  }
  and t_l2 = {
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

              nullableOfNullable: {
                let value =
                  Js.Dict.unsafeGet(Obj.magic(value), "nullableOfNullable");

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

              nullableOfNullable: {
                let value =
                  Js.Dict.unsafeGet(Obj.magic(value), "nullableOfNullable");

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

        l1: {
          let value = (value: t).l1;
          Js.Json.null;
        },

        l2: {
          let value = (value: t).l2;
          let initial: Js.Json.t = Obj.magic({});
          Js.Array.reduce(
            Graphql_PPX.deepMerge,
            initial,
            [|
              Fragments.ListFragment.serialize(value##frag1),
              Fragments.ListFragment.serialize(value##frag2),
            |],
          );
        },

        l3: {
          let value = (value: t).l3;
          let initial: Js.Json.t =
            Obj.magic({

              "nullableOfNullable": {
                let value = value##nullableOfNullable;

                switch (value) {
                | Some(value) =>
                  Js.Nullable.return(
                    generate_serializer(config, path, definition, inner),
                  )
                | None => Js.Nullable.null
                };
              },
            });
          Js.Array.reduce(
            Graphql_PPX.deepMerge,
            initial,
            [|
              Fragments.ListFragment.serialize(value##frag1),
              Fragments.ListFragment.serialize(value##frag2),
            |],
          );
        },

        l4: {
          let value = (value: t).l4;
          let initial: Js.Json.t =
            Obj.magic({

              "nullableOfNullable": {
                let value = value##nullableOfNullable;

                switch (value) {
                | Some(value) =>
                  Js.Nullable.return(
                    generate_serializer(config, path, definition, inner),
                  )
                | None => Js.Nullable.null
                };
              },
            });
          Js.Array.reduce(
            Graphql_PPX.deepMerge,
            initial,
            [|Fragments.ListFragment.serialize(value##listFragment)|],
          );
        },
      }: Raw.tt
    );
  let makeVar = (~f, ()) => f(Js.Json.null);
  let definition = (parse, query, makeVar);
};

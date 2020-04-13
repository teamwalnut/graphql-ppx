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
type scalars = {
  string,
  int,
};

type dog = {
  name: string,
  barkVolume: float,
};

type oneFieldQuery = {nullableString: option(string)};

module MyQuery = {
  module Raw = {
    type t = {variousScalars: scalars};
  };
  let query = "query   {\nvariousScalars  {\nstring  \nint  \n}\n\n}\n";
  type t = {variousScalars: scalars};
  let parse: Raw.t => t =
    (value) => (
      {

        variousScalars: {
          let value = (value: Raw.t).variousScalars;
          (
            {

              string: {
                let value = (value: Raw.t_variousScalars).string;

                value;
              },

              int: {
                let value = (value: Raw.t_variousScalars).int;

                value;
              },
            }: scalars
          );
        },
      }: t
    );
  let makeVar = (~f, ()) => f(Js.Json.null);
  let definition = (parse, query, makeVar);
};

module OneFieldQuery = {
  module Raw = {
    type t = {variousScalars: t_variousScalars}
    and t_variousScalars = {nullableString: Js.Nullable.t(string)};
  };
  let query = "query   {\nvariousScalars  {\nnullableString  \n}\n\n}\n";
  type t = {variousScalars: t_variousScalars}
  and t_variousScalars = {nullableString: option(string)};
  let parse: Raw.t => t =
    (value) => (
      {

        variousScalars: {
          let value = (value: Raw.t).variousScalars;
          (
            {

              nullableString: {
                let value = (value: Raw.t_variousScalars).nullableString;

                switch (Js.toOption(value)) {
                | Some(value) => Some(value)
                | None => None
                };
              },
            }: t_variousScalars
          );
        },
      }: t
    );
  let makeVar = (~f, ()) => f(Js.Json.null);
  let definition = (parse, query, makeVar);
};

module ExternalFragmentQuery = {
  module Fragment = {
    let query = "fragment Fragment on VariousScalars   {\nstring  \nint  \n}\n";
    module Raw = {
      type t = {
        string,
        int,
      };
      type t_VariousScalars = t;
    };
    type t = {
      string,
      int,
    };
    type t_VariousScalars = t;

    let parse = (value: Raw.t): t => {

      string: {
        let value = (value: Raw.t).string;

        value;
      },

      int: {
        let value = (value: Raw.t).int;

        value;
      },
    };
    let name = "Fragment";
  };
  module Untitled1 = {
    module Raw = {
      type t = {variousScalars: Fragment.Raw.t};
    };
    let query =
      (
        ("query   {\nvariousScalars  {\n..." ++ Fragment.name)
        ++ "   \n}\n\n}\n"
      )
      ++ Fragment.query;
    type t = {variousScalars: Fragment.t};
    let parse: Raw.t => t =
      (value) => (
        {

          variousScalars: {
            let value = (value: Raw.t).variousScalars;

            Fragment.parse(value);
          },
        }: t
      );
    let makeVar = (~f, ()) => f(Js.Json.null);
    let definition = (parse, query, makeVar);
  };
};

module InlineFragmentQuery = {
  module Raw = {
    type t = {dogOrHuman: t_dogOrHuman}
    and t_dogOrHuman
    and t_dogOrHuman_Dog = {
      name: string,
      barkVolume: float,
    };
  };
  let query = "query   {\ndogOrHuman  {\n__typename\n...on Dog   {\nname  \nbarkVolume  \n}\n\n}\n\n}\n";
  type t = {dogOrHuman: t_dogOrHuman}
  and t_dogOrHuman = [
    | `FutureAddedValue(Js.Json.t)
    | `Dog(t_dogOrHuman_Dog)
  ]
  and t_dogOrHuman_Dog = {
    name: string,
    barkVolume: float,
  };
  let parse: Raw.t => t =
    (value) => (
      {

        dogOrHuman: {
          let value = (value: Raw.t).dogOrHuman;

          switch (Js.Json.decodeObject(Obj.magic(value): Js.Json.t)) {

          | None =>
            Js.Exn.raiseError(
              "graphql_ppx: "
              ++ "Expected union "
              ++ "DogOrHuman"
              ++ " to be an object, got "
              ++ Js.Json.stringify(Obj.magic(value): Js.Json.t),
            )

          | Some(typename_obj) =>
            switch (Js.Dict.get(typename_obj, "__typename")) {

            | None =>
              Js.Exn.raiseError(
                "graphql_ppx: "
                ++ "Union "
                ++ "DogOrHuman"
                ++ " is missing the __typename field",
              )

            | Some(typename) =>
              switch (Js.Json.decodeString(typename)) {

              | None =>
                Js.Exn.raiseError(
                  "graphql_ppx: "
                  ++ "Union "
                  ++ "DogOrHuman"
                  ++ " has a __typename field that is not a string",
                )

              | Some(typename) =>
                switch (typename) {
                | "Dog" =>
                  `Dog(
                    {
                      let value: Raw.t_dogOrHuman_Dog = Obj.magic(value);
                      (
                        {

                          name: {
                            let value = (value: Raw.t_dogOrHuman_Dog).name;

                            value;
                          },

                          barkVolume: {
                            let value =
                              (value: Raw.t_dogOrHuman_Dog).barkVolume;

                            value;
                          },
                        }: t_dogOrHuman_Dog
                      );
                    },
                  )
                | _ => `FutureAddedValue(Obj.magic(value): Js.Json.t)
                }
              }
            }
          };
        },
      }: t
    );
  let makeVar = (~f, ()) => f(Js.Json.null);
  let definition = (parse, query, makeVar);
};

module UnionExternalFragmentQuery = {
  module DogFragment = {
    let query = "fragment DogFragment on Dog   {\nname  \nbarkVolume  \n}\n";
    module Raw = {
      type t = {
        name: string,
        barkVolume: float,
      };
      type t_Dog = t;
    };
    type t = {
      name: string,
      barkVolume: float,
    };
    type t_Dog = t;

    let parse = (value: Raw.t): t => {

      name: {
        let value = (value: Raw.t).name;

        value;
      },

      barkVolume: {
        let value = (value: Raw.t).barkVolume;

        value;
      },
    };
    let name = "DogFragment";
  };
  module Untitled1 = {
    module Raw = {
      type t = {dogOrHuman: t_dogOrHuman}
      and t_dogOrHuman;
    };
    let query =
      (
        (
          "query   {\ndogOrHuman  {\n__typename\n...on Dog   {\n..."
          ++ DogFragment.name
        )
        ++ "   \n}\n\n}\n\n}\n"
      )
      ++ DogFragment.query;
    type t = {dogOrHuman: t_dogOrHuman}
    and t_dogOrHuman = [
      | `FutureAddedValue(Js.Json.t)
      | `Dog(DogFragment.t)
    ];
    let parse: Raw.t => t =
      (value) => (
        {

          dogOrHuman: {
            let value = (value: Raw.t).dogOrHuman;

            switch (Js.Json.decodeObject(Obj.magic(value): Js.Json.t)) {

            | None =>
              Js.Exn.raiseError(
                "graphql_ppx: "
                ++ "Expected union "
                ++ "DogOrHuman"
                ++ " to be an object, got "
                ++ Js.Json.stringify(Obj.magic(value): Js.Json.t),
              )

            | Some(typename_obj) =>
              switch (Js.Dict.get(typename_obj, "__typename")) {

              | None =>
                Js.Exn.raiseError(
                  "graphql_ppx: "
                  ++ "Union "
                  ++ "DogOrHuman"
                  ++ " is missing the __typename field",
                )

              | Some(typename) =>
                switch (Js.Json.decodeString(typename)) {

                | None =>
                  Js.Exn.raiseError(
                    "graphql_ppx: "
                    ++ "Union "
                    ++ "DogOrHuman"
                    ++ " has a __typename field that is not a string",
                  )

                | Some(typename) =>
                  switch (typename) {
                  | "Dog" =>
                    `Dog(
                      {
                        let value: Raw.t_dogOrHuman_Dog = Obj.magic(value);

                        DogFragment.parse(value);
                      },
                    )
                  | _ => `FutureAddedValue(Obj.magic(value): Js.Json.t)
                  }
                }
              }
            };
          },
        }: t
      );
    let makeVar = (~f, ()) => f(Js.Json.null);
    let definition = (parse, query, makeVar);
  };
};

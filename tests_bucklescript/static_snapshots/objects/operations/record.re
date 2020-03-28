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
  let query = "query   {\nvariousScalars  {\nstring  \nint  \n}\n\n}\n";
  type raw_t;
  type t = {. "variousScalars": scalars};
  let parse: Js.Json.t => t =
    value => {
      [@metaloc loc]
      let value = value |> Js.Json.decodeObject |> Js.Option.getExn;
      {

        "variousScalars": {
          let value = Js.Dict.unsafeGet(Obj.magic(value), "variousScalars");
          [@metaloc loc]
          let value = value |> Js.Json.decodeObject |> Js.Option.getExn;
          {

            "string": {
              let value = Js.Dict.unsafeGet(Obj.magic(value), "string");

              (Obj.magic(value): string);
            },

            "int": {
              let value = Js.Dict.unsafeGet(Obj.magic(value), "int");

              (Obj.magic(value): int);
            },
          };
        },
      };
    };
  let makeVar = (~f, ()) => f(Js.Json.null);
  let definition = (parse, query, makeVar);
};

module OneFieldQuery = {
  let query = "query   {\nvariousScalars  {\nnullableString  \n}\n\n}\n";
  type raw_t;
  type t = {. "variousScalars": t_variousScalars}
  and t_variousScalars = {nullableString: option(string)};
  let parse: Js.Json.t => t =
    value => {
      [@metaloc loc]
      let value = value |> Js.Json.decodeObject |> Js.Option.getExn;
      {

        "variousScalars": {
          let value = Js.Dict.unsafeGet(Obj.magic(value), "variousScalars");
          [@metaloc loc]
          let value = value |> Js.Json.decodeObject |> Js.Option.getExn;
          {

            "nullableString": {
              let value =
                Js.Dict.unsafeGet(Obj.magic(value), "nullableString");

              switch (Js.toOption(Obj.magic(value): Js.Nullable.t('a))) {
              | Some(_) => Some(Obj.magic(value): string)
              | None => None
              };
            },
          };
        },
      };
    };
  let makeVar = (~f, ()) => f(Js.Json.null);
  let definition = (parse, query, makeVar);
};

module ExternalFragmentQuery = {
  module Fragment = {
    let query = "fragment Fragment on VariousScalars   {\nstring  \nint  \n}\n";
    type t = {
      string,
      int,
    };
    type raw_t;
    type t_VariousScalars = t;

    let parse = (value: Js.Json.t) => {
      [@metaloc loc]
      let value = value |> Js.Json.decodeObject |> Js.Option.getExn;
      {

        "string": {
          let value = Js.Dict.unsafeGet(Obj.magic(value), "string");

          (Obj.magic(value): string);
        },

        "int": {
          let value = Js.Dict.unsafeGet(Obj.magic(value), "int");

          (Obj.magic(value): int);
        },
      };
    };
    let name = "Fragment";
  };
  module Untitled1 = {
    let query =
      (
        ("query   {\nvariousScalars  {\n..." ++ Fragment.name)
        ++ "   \n}\n\n}\n"
      )
      ++ Fragment.query;
    type raw_t;
    type t = {. "variousScalars": Fragment.t};
    let parse: Js.Json.t => t =
      value => {
        [@metaloc loc]
        let value = value |> Js.Json.decodeObject |> Js.Option.getExn;
        {

          "variousScalars": {
            let value =
              Js.Dict.unsafeGet(Obj.magic(value), "variousScalars");

            Fragment.parse(value);
          },
        };
      };
    let makeVar = (~f, ()) => f(Js.Json.null);
    let definition = (parse, query, makeVar);
  };
};

module InlineFragmentQuery = {
  let query = "query   {\ndogOrHuman  {\n__typename\n...on Dog   {\nname  \nbarkVolume  \n}\n\n}\n\n}\n";
  type raw_t;
  type t = {
    .
    "dogOrHuman": [ | `FutureAddedValue(Js.Json.t) | `Dog(t_dogOrHuman_Dog)],
  }
  and t_dogOrHuman_Dog = {
    name: string,
    barkVolume: float,
  };
  let parse: Js.Json.t => t =
    value => {
      [@metaloc loc]
      let value = value |> Js.Json.decodeObject |> Js.Option.getExn;
      {

        "dogOrHuman": {
          let value = Js.Dict.unsafeGet(Obj.magic(value), "dogOrHuman");

          switch (Js.Json.decodeObject(value)) {

          | None =>
            Js.Exn.raiseError(
              "graphql_ppx: "
              ++ "Expected union "
              ++ "DogOrHuman"
              ++ " to be an object, got "
              ++ Js.Json.stringify(value),
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
                      [@metaloc loc]
                      let value =
                        value |> Js.Json.decodeObject |> Js.Option.getExn;
                      {

                        "name": {
                          let value =
                            Js.Dict.unsafeGet(Obj.magic(value), "name");

                          (Obj.magic(value): string);
                        },

                        "barkVolume": {
                          let value =
                            Js.Dict.unsafeGet(
                              Obj.magic(value),
                              "barkVolume",
                            );

                          (Obj.magic(value): float);
                        },
                      };
                    },
                  )
                | typename => `FutureAddedValue(value)
                }
              }
            }
          };
        },
      };
    };
  let makeVar = (~f, ()) => f(Js.Json.null);
  let definition = (parse, query, makeVar);
};

module UnionExternalFragmentQuery = {
  module DogFragment = {
    let query = "fragment DogFragment on Dog   {\nname  \nbarkVolume  \n}\n";
    type t = {
      name: string,
      barkVolume: float,
    };
    type raw_t;
    type t_Dog = t;

    let parse = (value: Js.Json.t) => {
      [@metaloc loc]
      let value = value |> Js.Json.decodeObject |> Js.Option.getExn;
      {

        "name": {
          let value = Js.Dict.unsafeGet(Obj.magic(value), "name");

          (Obj.magic(value): string);
        },

        "barkVolume": {
          let value = Js.Dict.unsafeGet(Obj.magic(value), "barkVolume");

          (Obj.magic(value): float);
        },
      };
    };
    let name = "DogFragment";
  };
  module Untitled1 = {
    let query =
      (
        (
          "query   {\ndogOrHuman  {\n__typename\n...on Dog   {\n..."
          ++ DogFragment.name
        )
        ++ "   \n}\n\n}\n\n}\n"
      )
      ++ DogFragment.query;
    type raw_t;
    type t = {
      .
      "dogOrHuman": [ | `FutureAddedValue(Js.Json.t) | `Dog(DogFragment.t)],
    };
    let parse: Js.Json.t => t =
      value => {
        [@metaloc loc]
        let value = value |> Js.Json.decodeObject |> Js.Option.getExn;
        {

          "dogOrHuman": {
            let value = Js.Dict.unsafeGet(Obj.magic(value), "dogOrHuman");

            switch (Js.Json.decodeObject(value)) {

            | None =>
              Js.Exn.raiseError(
                "graphql_ppx: "
                ++ "Expected union "
                ++ "DogOrHuman"
                ++ " to be an object, got "
                ++ Js.Json.stringify(value),
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
                  | "Dog" => `Dog(DogFragment.parse(value))
                  | typename => `FutureAddedValue(value)
                  }
                }
              }
            };
          },
        };
      };
    let makeVar = (~f, ()) => f(Js.Json.null);
    let definition = (parse, query, makeVar);
  };
};

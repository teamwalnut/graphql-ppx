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
    type t = {. "dogOrHuman": t_dogOrHuman}
    and t_dogOrHuman = {__typename: string}
    and t_dogOrHuman_Human = {. "name": string}
    and t_dogOrHuman_Dog = {
      .
      "name": string,
      "barkVolume": float,
    };
  };
  let query = "query   {\ndogOrHuman  {\n__typename\n...on Dog   {\nname  \nbarkVolume  \n}\n\n...on Human   {\nname  \n}\n\n}\n\n}\n";
  type t = {. "dogOrHuman": t_dogOrHuman}
  and t_dogOrHuman = [
    | `FutureAddedValue(Js.Json.t)
    | `Dog(t_dogOrHuman_Dog)
    | `Human(t_dogOrHuman_Human)
  ]
  and t_dogOrHuman_Human = {. "name": string}
  and t_dogOrHuman_Dog = {
    .
    "name": string,
    "barkVolume": float,
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
                | "Human" =>
                  `Human(
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

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
    type t_mutationForVariant_dog = {
      .
      "__typename": string,
      "name": string,
      "barkVolume": float,
    };
    type t_mutationForVariant_human = {
      .
      "__typename": string,
      "name": string,
    };
    type t_mutationForVariant_dogOrHuman_Dog = {
      .
      "__typename": string,
      "name": string,
      "barkVolume": float,
    };
    type t_mutationForVariant_dogOrHuman_Human = {
      .
      "__typename": string,
      "name": string,
    };
    type t_mutationForVariant_dogOrHuman;
    type t_mutationForVariant;
    type t = {. "mutationForVariant": t_mutationForVariant};
  };
  let query = "mutation   {\nmutationForVariant  {\nbaseType  \nbaseTypeList  \ndog  {\nname  \nbarkVolume  \n}\n\nhuman  {\nname  \n}\n\ndogOrHuman  {\n__typename\n...on Dog   {\nname  \nbarkVolume  \n}\n\n...on Human   {\nname  \n}\n\n}\n\n}\n\n}\n";
  type t_mutationForVariant_dog = {
    .
    "__typename": string,
    "name": string,
    "barkVolume": float,
  };
  type t_mutationForVariant_human = {
    .
    "__typename": string,
    "name": string,
  };
  type t_mutationForVariant_dogOrHuman_Dog = {
    .
    "__typename": string,
    "name": string,
    "barkVolume": float,
  };
  type t_mutationForVariant_dogOrHuman_Human = {
    .
    "__typename": string,
    "name": string,
  };
  type t_mutationForVariant_dogOrHuman = [
    | `FutureAddedValue(Js.Json.t)
    | `Dog(t_mutationForVariant_dogOrHuman_Dog)
    | `Human(t_mutationForVariant_dogOrHuman_Human)
  ];
  type t_mutationForVariant = [
    | `BaseType(string)
    | `BaseTypeList(array(string))
    | `Dog(t_mutationForVariant_dog)
    | `Human(t_mutationForVariant_human)
    | `DogOrHuman(t_mutationForVariant_dogOrHuman)
  ];
  type t = {. "mutationForVariant": t_mutationForVariant};
  let parse: Raw.t => t =
    value => {
      let mutationForVariant = {
        let value = value##mutationForVariant;
        switch (Js.Json.decodeObject(Obj.magic(value): Js.Json.t)) {

        | None =>
          Js.Exn.raiseError(
            "graphql_ppx: "
            ++ "Expected type "
            ++ "VariantTestResult"
            ++ " to be an object",
          )

        | Some(value) =>
          let temp = Js.Dict.unsafeGet(Obj.magic(value), "baseType");
          switch (Js.Json.decodeNull(temp)) {
          | None =>
            let value = temp;
            `BaseType(value);
          | Some(_) =>
            let temp = Js.Dict.unsafeGet(Obj.magic(value), "baseTypeList");
            switch (Js.Json.decodeNull(temp)) {
            | None =>
              let value = temp;
              `BaseTypeList(value |> Js.Array.map(value => value));
            | Some(_) =>
              let temp = Js.Dict.unsafeGet(Obj.magic(value), "dog");
              switch (Js.Json.decodeNull(temp)) {
              | None =>
                let value = temp;
                `Dog(
                  {
                    let barkVolume = {
                      let value = value##barkVolume;
                      value;
                    }
                    and name = {
                      let value = value##name;
                      value;
                    };
                    {

                      "name": name,

                      "barkVolume": barkVolume,
                    };
                  },
                );
              | Some(_) =>
                let temp = Js.Dict.unsafeGet(Obj.magic(value), "human");
                switch (Js.Json.decodeNull(temp)) {
                | None =>
                  let value = temp;
                  `Human(
                    {
                      let name = {
                        let value = value##name;
                        value;
                      };
                      {

                        "name": name,
                      };
                    },
                  );
                | Some(_) =>
                  let temp =
                    Js.Dict.unsafeGet(Obj.magic(value), "dogOrHuman");
                  switch (Js.Json.decodeNull(temp)) {
                  | None =>
                    let value = temp;
                    `DogOrHuman(
                      {
                        let typename: string =
                          Obj.magic(
                            Js.Dict.unsafeGet(
                              Obj.magic(value),
                              "__typename",
                            ),
                          );
                        (
                          switch (typename) {
                          | "Dog" =>
                            `Dog(
                              {
                                let value: Raw.t_mutationForVariant_dogOrHuman_Dog =
                                  Obj.magic(value);
                                let barkVolume = {
                                  let value = value##barkVolume;
                                  value;
                                }
                                and name = {
                                  let value = value##name;
                                  value;
                                };
                                {

                                  "name": name,

                                  "barkVolume": barkVolume,
                                };
                              },
                            )
                          | "Human" =>
                            `Human(
                              {
                                let value: Raw.t_mutationForVariant_dogOrHuman_Human =
                                  Obj.magic(value);
                                let name = {
                                  let value = value##name;
                                  value;
                                };
                                {

                                  "name": name,
                                };
                              },
                            )
                          | _ =>
                            `FutureAddedValue(Obj.magic(value): Js.Json.t)
                          }: t_mutationForVariant_dogOrHuman
                        );
                      },
                    );
                  | Some(_) =>
                    Js.Exn.raiseError(
                      "graphql_ppx: "
                      ++ "All fields on variant selection set on type "
                      ++ "VariantTestResult"
                      ++ " were null",
                    )
                  };
                };
              };
            };
          };
        };
      };
      {

        "mutationForVariant": mutationForVariant,
      };
    };
  let serialize: t => Raw.t =
    value => {
      let mutationForVariant = {
        let value = value##mutationForVariant;
        Js.Json.null;
      };
      {

        "mutationForVariant": mutationForVariant,
      };
    };
  let definition = (parse, query, serialize);
};

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
    type t_variables = unit;
  };
  /**The GraphQL query string*/
  let query = "mutation   {\nmutationForVariant  {\nbaseType  \nbaseTypeList  \ndog  {\nname  \nbarkVolume  \n}\n\nhuman  {\nname  \n}\n\ndogOrHuman  {\n__typename\n...on Dog   {\nname  \nbarkVolume  \n}\n\n...on Human   {\nname  \n}\n\n}\n\n}\n\n}\n";
  type t_mutationForVariant_dog = {
    .
    "name": string,
    "barkVolume": float,
  };
  type t_mutationForVariant_human = {. "name": string};
  type t_mutationForVariant_dogOrHuman_Dog = {
    .
    "name": string,
    "barkVolume": float,
  };
  type t_mutationForVariant_dogOrHuman_Human = {. "name": string};
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
  type t_variables = unit;
  /**Parse the JSON GraphQL data to ReasonML data types*/
  let parse = (value: Raw.t): t => {
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
                  {"name": name, "barkVolume": barkVolume};
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
                    {"name": name};
                  },
                );
              | Some(_) =>
                let temp = Js.Dict.unsafeGet(Obj.magic(value), "dogOrHuman");
                switch (Js.Json.decodeNull(temp)) {
                | None =>
                  let value = temp;
                  `DogOrHuman(
                    {
                      let typename: string =
                        Obj.magic(
                          Js.Dict.unsafeGet(Obj.magic(value), "__typename"),
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
                              {"name": name, "barkVolume": barkVolume};
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
                              {"name": name};
                            },
                          )
                        | _ => `FutureAddedValue(Obj.magic(value): Js.Json.t)
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
    {"mutationForVariant": mutationForVariant};
  };
  /**Serialize the ReasonML GraphQL data that was parsed using the parse function back to the original JSON compatible data */
  let serialize = (value: t): Raw.t => {
    let mutationForVariant = {
      let value = value##mutationForVariant;
      Obj.magic(Js.Json.null);
    };
    {"mutationForVariant": mutationForVariant};
  };
  let makeVariables = () => ();
  let makeDefaultVariables = () => makeVariables();
  let make = () => {
    "query": query,
    "variables": Js.Json.null,
    "parse": parse,
  };
  external unsafe_fromJson: Js.Json.t => Raw.t = "%identity";
  external toJson: Raw.t => Js.Json.t = "%identity";
  module Z__INTERNAL = {
    type root = t;
    type nonrec graphql_module;
    /****--- graphql-ppx module ---**

The contents of this module are automatically generated by `graphql-ppx`.
The following is simply an overview of the most important variables and types that you can access from this module.

```
module MyQuery {
  /**
  The GraphQL query string
  */
  let query: string;

  /**
  This is the main type of the result you will get back.
  You can hover above the identifier key (e.g. query or mutation) to see the fully generated type for your module.
  */
  type t;

  /**
  Parse the JSON GraphQL data to ReasonML data types
  */
  let parse: Raw.t => t;

  /**
  Serialize the ReasonML GraphQL data that was parsed using the parse function back to the original JSON compatible data
  */
  let serialize: t => Raw.t;

  /**
  This is the JSON compatible type of the GraphQL data.
  It should not be necessary to access the types inside for normal use cases.
  */
  module Raw: { type t; };
}
```*/
    let graphql_module: graphql_module = Obj.magic(0);
  };
};

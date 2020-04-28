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
    type t_dogOrHuman_Dog = {
      __typename: string,
      name: string,
      barkVolume: float,
    };
    type t_dogOrHuman_Human = {
      __typename: string,
      name: string,
    };
    type t_dogOrHuman;
    type t = {dogOrHuman: t_dogOrHuman};
  };
  let query = "query   {\ndogOrHuman  {\n__typename\n...on Dog   {\n__typename  \nname  \nbarkVolume  \n}\n\n...on Human   {\n__typename  \nname  \n}\n\n}\n\n}\n";
  type t_dogOrHuman_Dog = {
    __typename: string,
    name: string,
    barkVolume: float,
  };
  type t_dogOrHuman_Human = {
    __typename: string,
    name: string,
  };
  type t_dogOrHuman = [
    | `FutureAddedValue(Js.Json.t)
    | `Dog(t_dogOrHuman_Dog)
    | `Human(t_dogOrHuman_Human)
  ];
  type t = {dogOrHuman: t_dogOrHuman};
  let parse: Raw.t => t =
    (value) => (
      {
        dogOrHuman: {
          let value = (value: Raw.t).dogOrHuman;
          let typename: string =
            Obj.magic(Js.Dict.unsafeGet(Obj.magic(value), "__typename"));
          (
            switch (typename) {
            | "Dog" =>
              `Dog(
                {
                  let value: Raw.t_dogOrHuman_Dog = Obj.magic(value);
                  (
                    {
                      __typename: {
                        let value = (value: Raw.t_dogOrHuman_Dog).__typename;
                        value;
                      },
                      name: {
                        let value = (value: Raw.t_dogOrHuman_Dog).name;
                        value;
                      },
                      barkVolume: {
                        let value = (value: Raw.t_dogOrHuman_Dog).barkVolume;
                        value;
                      },
                    }: t_dogOrHuman_Dog
                  );
                },
              )
            | "Human" =>
              `Human(
                {
                  let value: Raw.t_dogOrHuman_Human = Obj.magic(value);
                  (
                    {
                      __typename: {
                        let value = (value: Raw.t_dogOrHuman_Human).__typename;
                        value;
                      },
                      name: {
                        let value = (value: Raw.t_dogOrHuman_Human).name;
                        value;
                      },
                    }: t_dogOrHuman_Human
                  );
                },
              )
            | _ => `FutureAddedValue(Obj.magic(value): Js.Json.t)
            }: t_dogOrHuman
          );
        },
      }: t
    );
  let serialize: t => Raw.t =
    (value) => (
      {
        let dogOrHuman = {
          let value = (value: t).dogOrHuman;
          switch (value) {
          | `Dog(value) => (
              Obj.magic(
                {
                  let barkVolume = {
                    let value = (value: t_dogOrHuman_Dog).barkVolume;

                    value;
                  }
                  and name = {
                    let value = (value: t_dogOrHuman_Dog).name;

                    value;
                  }
                  and __typename = {
                    let value = (value: t_dogOrHuman_Dog).__typename;

                    value;
                  };
                  {

                    __typename: "Dog",

                    name,

                    barkVolume,
                  };
                }: Raw.t_dogOrHuman_Dog,
              ): Raw.t_dogOrHuman
            )
          | `Human(value) => (
              Obj.magic(
                {
                  let name = {
                    let value = (value: t_dogOrHuman_Human).name;

                    value;
                  }
                  and __typename = {
                    let value = (value: t_dogOrHuman_Human).__typename;

                    value;
                  };
                  {

                    __typename: "Human",

                    name,
                  };
                }: Raw.t_dogOrHuman_Human,
              ): Raw.t_dogOrHuman
            )
          | `FutureAddedValue(value) => (Obj.magic(value): Raw.t_dogOrHuman)
          };
        };
        {

          dogOrHuman: dogOrHuman,
        };
      }: Raw.t
    );
  let definition = (parse, query, serialize);
};

module MyQueryNoError = {
  module Raw = {
    type t_dogOrHuman_Dog = {
      __typename: string,
      name: string,
      barkVolume: float,
    };
    type t_dogOrHuman_Human = {
      __typename: string,
      name: string,
    };
    type t_dogOrHuman;
    type t = {dogOrHuman: t_dogOrHuman};
  };
  let query = "query   {\ndogOrHuman  {\n__typename\n...on Dog   {\n__typename  \nname  \nbarkVolume  \n}\n\n...on Human   {\n__typename  \nname  \n}\n\n}\n\n}\n";
  type t_dogOrHuman_Dog = {
    __typename: string,
    name: string,
    barkVolume: float,
  };
  type t_dogOrHuman_Human = {
    __typename: string,
    name: string,
  };
  type t_dogOrHuman = [
    | `FutureAddedValue(Js.Json.t)
    | `Dog(t_dogOrHuman_Dog)
    | `Human(t_dogOrHuman_Human)
  ];
  type t = {dogOrHuman: t_dogOrHuman};
  let parse: Raw.t => t =
    (value) => (
      {
        dogOrHuman: {
          let value = (value: Raw.t).dogOrHuman;
          let typename: string =
            Obj.magic(Js.Dict.unsafeGet(Obj.magic(value), "__typename"));
          (
            switch (typename) {
            | "Dog" =>
              `Dog(
                {
                  let value: Raw.t_dogOrHuman_Dog = Obj.magic(value);
                  (
                    {
                      __typename: {
                        let value = (value: Raw.t_dogOrHuman_Dog).__typename;
                        value;
                      },
                      name: {
                        let value = (value: Raw.t_dogOrHuman_Dog).name;
                        value;
                      },
                      barkVolume: {
                        let value = (value: Raw.t_dogOrHuman_Dog).barkVolume;
                        value;
                      },
                    }: t_dogOrHuman_Dog
                  );
                },
              )
            | "Human" =>
              `Human(
                {
                  let value: Raw.t_dogOrHuman_Human = Obj.magic(value);
                  (
                    {
                      __typename: {
                        let value = (value: Raw.t_dogOrHuman_Human).__typename;
                        value;
                      },
                      name: {
                        let value = (value: Raw.t_dogOrHuman_Human).name;
                        value;
                      },
                    }: t_dogOrHuman_Human
                  );
                },
              )
            | _ => `FutureAddedValue(Obj.magic(value): Js.Json.t)
            }: t_dogOrHuman
          );
        },
      }: t
    );
  let serialize: t => Raw.t =
    (value) => (
      {
        let dogOrHuman = {
          let value = (value: t).dogOrHuman;
          switch (value) {
          | `Dog(value) => (
              Obj.magic(
                {
                  let barkVolume = {
                    let value = (value: t_dogOrHuman_Dog).barkVolume;

                    value;
                  }
                  and name = {
                    let value = (value: t_dogOrHuman_Dog).name;

                    value;
                  }
                  and __typename = {
                    let value = (value: t_dogOrHuman_Dog).__typename;

                    value;
                  };
                  {

                    __typename: "Dog",

                    name,

                    barkVolume,
                  };
                }: Raw.t_dogOrHuman_Dog,
              ): Raw.t_dogOrHuman
            )
          | `Human(value) => (
              Obj.magic(
                {
                  let name = {
                    let value = (value: t_dogOrHuman_Human).name;

                    value;
                  }
                  and __typename = {
                    let value = (value: t_dogOrHuman_Human).__typename;

                    value;
                  };
                  {

                    __typename: "Human",

                    name,
                  };
                }: Raw.t_dogOrHuman_Human,
              ): Raw.t_dogOrHuman
            )
          | `FutureAddedValue(value) => (Obj.magic(value): Raw.t_dogOrHuman)
          };
        };
        {

          dogOrHuman: dogOrHuman,
        };
      }: Raw.t
    );
  let definition = (parse, query, serialize);
};

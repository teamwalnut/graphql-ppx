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
    type t = {
      __typename: string,
      dogOrHuman: t_dogOrHuman,
    }
    and t_dogOrHuman
    and t_dogOrHuman_Dog = {
      name: string,
      barkVolume: float,
    };
  };
  let query = "query   {\n__typename  \ndogOrHuman  {\n__typename\n...on Dog   {\nname  \nbarkVolume  \n}\n\n}\n\n}\n";
  type t = {
    __typename: string,
    dogOrHuman: t_dogOrHuman,
  }
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

        __typename: {
          let value = (value: Raw.t).__typename;

          value;
        },

        dogOrHuman: {
          let value = (value: Raw.t).dogOrHuman;
          [@metaloc loc]
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
                  };
                  {

                    name,

                    barkVolume,
                  };
                }: Raw.t_dogOrHuman_Dog,
              ): Raw.t_dogOrHuman
            )
          | `FutureAddedValue(value) => (Obj.magic(value): Raw.t_dogOrHuman)
          };
        }
        and __typename = {
          let value = (value: t).__typename;

          value;
        };
        {

          __typename,

          dogOrHuman,
        };
      }: Raw.t
    );
  let makeVar = (~f, ()) => f(Js.Json.null);
  let definition = (parse, query, makeVar);
};

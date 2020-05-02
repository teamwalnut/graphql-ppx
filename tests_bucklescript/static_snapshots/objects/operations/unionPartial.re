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
      .
      "__typename": string,
      "name": string,
      "barkVolume": float,
    };
    type t_dogOrHuman;
    type t = {. "dogOrHuman": t_dogOrHuman};
  };
  let query = "query   {\ndogOrHuman  {\n__typename\n...on Dog   {\nname  \nbarkVolume  \n}\n\n}\n\n}\n";
  type t_dogOrHuman_Dog = {
    .
    "name": string,
    "barkVolume": float,
  };
  type t_dogOrHuman = [
    | `FutureAddedValue(Js.Json.t)
    | `Dog(t_dogOrHuman_Dog)
  ];
  type t = {. "dogOrHuman": t_dogOrHuman};
  let parse: Raw.t => t =
    value => {
      let dogOrHuman = {
        let value = value##dogOrHuman;
        let typename: string =
          Obj.magic(Js.Dict.unsafeGet(Obj.magic(value), "__typename"));
        (
          switch (typename) {
          | "Dog" =>
            `Dog(
              {
                let value: Raw.t_dogOrHuman_Dog = Obj.magic(value);
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
          | _ => `FutureAddedValue(Obj.magic(value): Js.Json.t)
          }: t_dogOrHuman
        );
      };
      {

        "dogOrHuman": dogOrHuman,
      };
    };
  let serialize: t => Raw.t =
    value => {
      let dogOrHuman = {
        let value = value##dogOrHuman;
        switch (value) {
        | `Dog(value) => (
            Obj.magic(
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

                  "__typename": "Dog",

                  "name": name,

                  "barkVolume": barkVolume,
                };
              },
            ): Raw.t_dogOrHuman
          )
        | `FutureAddedValue(value) => (Obj.magic(value): Raw.t_dogOrHuman)
        };
      };
      {

        "dogOrHuman": dogOrHuman,
      };
    };
  let definition = (parse, query, serialize);
};

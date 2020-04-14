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
    and t_dogOrHuman
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
  let parse: Raw.t => t =
    value => {

      "dogOrHuman": {
        let value = value##dogOrHuman;
        [@metaloc loc]
        let typename: string =
          Obj.magic(Js.Dict.unsafeGet(Obj.magic(value), "__typename"));
        (
          switch (typename) {
          | "Dog" =>
            `Dog(
              {
                let value: Raw.t_dogOrHuman_Dog = Obj.magic(value);
                {

                  "name": {
                    let value = value##name;

                    value;
                  },

                  "barkVolume": {
                    let value = value##barkVolume;

                    value;
                  },
                };
              },
            )
          | "Human" =>
            `Human(
              {
                let value: Raw.t_dogOrHuman_Human = Obj.magic(value);
                {

                  "name": {
                    let value = value##name;

                    value;
                  },
                };
              },
            )
          | _ => `FutureAddedValue(Obj.magic(value): Js.Json.t)
          }: t_dogOrHuman
        );
      },
    };
  let serialize: t => Raw.t =
    value => {

      "dogOrHuman": {
        let value = value##dogOrHuman;
        switch (value) {
        | `Dog(value) => (
            Obj.magic({

              "name": {
                let value = value##name;

                value;
              },

              "barkVolume": {
                let value = value##barkVolume;

                value;
              },
            }): Raw.t_dogOrHuman
          )
        | `Human(value) => (
            Obj.magic({

              "name": {
                let value = value##name;

                value;
              },
            }): Raw.t_dogOrHuman
          )
        | `FutureAddedValue(value) => (
            Obj.magic(ident_from_string("value")): Raw.t_dogOrHuman
          )
        };
      },
    };
  let makeVar = (~f, ()) => f(Js.Json.null);
  let make =
    makeVar(~f=variables =>
      {"query": query, "variables": variables, "parse": parse}
    );
  let makeWithVariables = variables => {
    "query": query,
    "variables": serializeVariables(variables),
    "parse": parse,
  };
  let definition = (parse, query, makeVar);
};

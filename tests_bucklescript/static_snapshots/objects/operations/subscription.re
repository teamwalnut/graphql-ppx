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
    type t_simpleSubscription_Dog = {
      .
      "__typename": string,
      "name": string,
    };
    type t_simpleSubscription_Human = {
      .
      "__typename": string,
      "name": string,
    };
    type t_simpleSubscription;
    type t = {. "simpleSubscription": t_simpleSubscription};
  };
  let query = "subscription   {\nsimpleSubscription  {\n__typename\n...on Dog   {\nname  \n}\n\n...on Human   {\nname  \n}\n\n}\n\n}\n";
  type t_simpleSubscription_Dog = {
    .
    "__typename": string,
    "name": string,
  };
  type t_simpleSubscription_Human = {
    .
    "__typename": string,
    "name": string,
  };
  type t_simpleSubscription = [
    | `FutureAddedValue(Js.Json.t)
    | `Dog(t_simpleSubscription_Dog)
    | `Human(t_simpleSubscription_Human)
  ];
  type t = {. "simpleSubscription": t_simpleSubscription};
  let parse: Raw.t => t =
    value => {

      "simpleSubscription": {
        let value = value##simpleSubscription;
        [@metaloc loc]
        let typename: string =
          Obj.magic(Js.Dict.unsafeGet(Obj.magic(value), "__typename"));
        (
          switch (typename) {
          | "Dog" =>
            `Dog(
              {
                let value: Raw.t_simpleSubscription_Dog = Obj.magic(value);
                {

                  "name": {
                    let value = value##name;

                    value;
                  },
                };
              },
            )
          | "Human" =>
            `Human(
              {
                let value: Raw.t_simpleSubscription_Human = Obj.magic(value);
                {

                  "name": {
                    let value = value##name;

                    value;
                  },
                };
              },
            )
          | _ => `FutureAddedValue(Obj.magic(value): Js.Json.t)
          }: t_simpleSubscription
        );
      },
    };
  let serialize: t => Raw.t =
    value => {
      let simpleSubscription = {
        let value = value##simpleSubscription;
        switch (value) {
        | `Dog(value) => (
            Obj.magic(
              {
                let name = {
                  let value = value##name;

                  value;
                };
                {

                  "__typename": "Dog",

                  "name": name,
                };
              },
            ): Raw.t_simpleSubscription
          )
        | `Human(value) => (
            Obj.magic(
              {
                let name = {
                  let value = value##name;

                  value;
                };
                {

                  "__typename": "Human",

                  "name": name,
                };
              },
            ): Raw.t_simpleSubscription
          )
        | `FutureAddedValue(value) => (
            Obj.magic(value): Raw.t_simpleSubscription
          )
        };
      };
      {

        "simpleSubscription": simpleSubscription,
      };
    };
  let definition = (parse, query, serialize);
};

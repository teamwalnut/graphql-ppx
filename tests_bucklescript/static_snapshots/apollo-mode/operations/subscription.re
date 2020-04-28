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
      __typename: string,
      name: string,
    };
    type t_simpleSubscription_Human = {
      __typename: string,
      name: string,
    };
    type t_simpleSubscription;
    type t = {simpleSubscription: t_simpleSubscription};
  };
  let query = "subscription   {\nsimpleSubscription  {\n__typename\n...on Dog   {\n__typename  \nname  \n}\n\n...on Human   {\n__typename  \nname  \n}\n\n}\n\n}\n";
  type t_simpleSubscription_Dog = {
    __typename: string,
    name: string,
  };
  type t_simpleSubscription_Human = {
    __typename: string,
    name: string,
  };
  type t_simpleSubscription = [
    | `FutureAddedValue(Js.Json.t)
    | `Dog(t_simpleSubscription_Dog)
    | `Human(t_simpleSubscription_Human)
  ];
  type t = {simpleSubscription: t_simpleSubscription};
  let parse: Raw.t => t =
    (value) => (
      {

        simpleSubscription: {
          let value = (value: Raw.t).simpleSubscription;
          [@metaloc loc]
          let typename: string =
            Obj.magic(Js.Dict.unsafeGet(Obj.magic(value), "__typename"));
          (
            switch (typename) {
            | "Dog" =>
              `Dog(
                {
                  let value: Raw.t_simpleSubscription_Dog = Obj.magic(value);
                  (
                    {

                      __typename: {
                        let value =
                          (value: Raw.t_simpleSubscription_Dog).__typename;

                        value;
                      },

                      name: {
                        let value = (value: Raw.t_simpleSubscription_Dog).name;

                        value;
                      },
                    }: t_simpleSubscription_Dog
                  );
                },
              )
            | "Human" =>
              `Human(
                {
                  let value: Raw.t_simpleSubscription_Human = Obj.magic(value);
                  (
                    {

                      __typename: {
                        let value =
                          (value: Raw.t_simpleSubscription_Human).__typename;

                        value;
                      },

                      name: {
                        let value =
                          (value: Raw.t_simpleSubscription_Human).name;

                        value;
                      },
                    }: t_simpleSubscription_Human
                  );
                },
              )
            | _ => `FutureAddedValue(Obj.magic(value): Js.Json.t)
            }: t_simpleSubscription
          );
        },
      }: t
    );
  let serialize: t => Raw.t =
    (value) => (
      {
        let simpleSubscription = {
          let value = (value: t).simpleSubscription;
          switch (value) {
          | `Dog(value) => (
              Obj.magic(
                {
                  let name = {
                    let value = (value: t_simpleSubscription_Dog).name;

                    value;
                  }
                  and __typename = {
                    let value = (value: t_simpleSubscription_Dog).__typename;

                    value;
                  };
                  {

                    __typename: "Dog",

                    name,
                  };
                }: Raw.t_simpleSubscription_Dog,
              ): Raw.t_simpleSubscription
            )
          | `Human(value) => (
              Obj.magic(
                {
                  let name = {
                    let value = (value: t_simpleSubscription_Human).name;

                    value;
                  }
                  and __typename = {
                    let value = (value: t_simpleSubscription_Human).__typename;

                    value;
                  };
                  {

                    __typename: "Human",

                    name,
                  };
                }: Raw.t_simpleSubscription_Human,
              ): Raw.t_simpleSubscription
            )
          | `FutureAddedValue(value) => (
              Obj.magic(value): Raw.t_simpleSubscription
            )
          };
        };
        {

          simpleSubscription: simpleSubscription,
        };
      }: Raw.t
    );
  let definition = (parse, query, serialize);
};

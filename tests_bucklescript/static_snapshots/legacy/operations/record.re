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
  module Raw = {
    type t = {. "variousScalars": scalars};
  };
  let query = "query   {\nvariousScalars  {\nstring  \nint  \n}\n\n}\n";
  type t = {. "variousScalars": scalars};
  let parse: Raw.t => t =
    value => {

      "variousScalars": {
        let value = value##variousScalars;
        {

          "string": {
            let value = value##string;

            value;
          },

          "int": {
            let value = value##int;

            value;
          },
        };
      },
    };
  let serialize: t => Raw.t =
    value => {
      let variousScalars = {
        let value = value##variousScalars;
        let int = {
          let value = value##int;

          value;
        }
        and string = {
          let value = value##string;

          value;
        };
        {

          "string": string,

          "int": int,
        };
      };
      {

        "variousScalars": variousScalars,
      };
    };
  let makeWithVariables = variables => {
    "query": query,
    "variables": serializeVariables(variables),
    "parse": parse,
  };
  let make = () => {
    "query": query,
    "variables": Js.Json.null,
    "parse": parse,
  };
  let definition = (parse, query, serialize);
};

module OneFieldQuery = {
  module Raw = {
    type t = {. "variousScalars": t_variousScalars}
    and t_variousScalars = {nullableString: Js.Nullable.t(string)};
  };
  let query = "query   {\nvariousScalars  {\nnullableString  \n}\n\n}\n";
  type t = {. "variousScalars": t_variousScalars}
  and t_variousScalars = {nullableString: option(string)};
  let parse: Raw.t => t =
    value => {

      "variousScalars": {
        let value = value##variousScalars;
        {

          "nullableString": {
            let value = value##nullableString;

            switch (Js.toOption(value)) {
            | Some(value) => Some(value)
            | None => None
            };
          },
        };
      },
    };
  let serialize: t => Raw.t =
    value => {
      let variousScalars = {
        let value = value##variousScalars;
        let nullableString = {
          let value = value##nullableString;

          switch (value) {
          | Some(value) => Js.Nullable.return(value)
          | None => Js.Nullable.null
          };
        };
        {

          "nullableString": nullableString,
        };
      };
      {

        "variousScalars": variousScalars,
      };
    };
  let makeWithVariables = variables => {
    "query": query,
    "variables": serializeVariables(variables),
    "parse": parse,
  };
  let make = () => {
    "query": query,
    "variables": Js.Json.null,
    "parse": parse,
  };
  let definition = (parse, query, serialize);
};

module ExternalFragmentQuery = {
  module Fragment = {
    let query = "fragment Fragment on VariousScalars   {\nstring  \nint  \n}\n";
    module Raw = {
      type t = {
        string,
        int,
      };
      type t_VariousScalars = t;
    };
    type t = {
      string,
      int,
    };
    type t_VariousScalars = t;

    let parse = (value: Raw.t) => {

      "string": {
        let value = value##string;

        value;
      },

      "int": {
        let value = value##int;

        value;
      },
    };
    let serialize: t => Raw.t =
      value => {
        let int = {
          let value = value##int;

          value;
        }
        and string = {
          let value = value##string;

          value;
        };
        {

          "string": string,

          "int": int,
        };
      };
    let name = "Fragment";
  };
  module Untitled1 = {
    module Raw = {
      type t = {. "variousScalars": Fragment.Raw.t};
    };
    let query =
      (
        ("query   {\nvariousScalars  {\n..." ++ Fragment.name)
        ++ "   \n}\n\n}\n"
      )
      ++ Fragment.query;
    type t = {. "variousScalars": Fragment.t};
    let parse: Raw.t => t =
      value => {

        "variousScalars": {
          let value = value##variousScalars;

          Fragment.parse(value);
        },
      };
    let serialize: t => Raw.t =
      value => {
        let variousScalars = {
          let value = value##variousScalars;
          Fragment.serialize(value);
        };
        {

          "variousScalars": variousScalars,
        };
      };
    let makeWithVariables = variables => {
      "query": query,
      "variables": serializeVariables(variables),
      "parse": parse,
    };
    let make = () => {
      "query": query,
      "variables": Js.Json.null,
      "parse": parse,
    };
    let definition = (parse, query, serialize);
  };
};

module InlineFragmentQuery = {
  module Raw = {
    type t = {. "dogOrHuman": t_dogOrHuman}
    and t_dogOrHuman
    and t_dogOrHuman_Dog = {
      name: string,
      barkVolume: float,
    };
  };
  let query = "query   {\ndogOrHuman  {\n__typename\n...on Dog   {\nname  \nbarkVolume  \n}\n\n}\n\n}\n";
  type t = {. "dogOrHuman": t_dogOrHuman}
  and t_dogOrHuman = [
    | `FutureAddedValue(Js.Json.t)
    | `Dog(t_dogOrHuman_Dog)
  ]
  and t_dogOrHuman_Dog = {
    name: string,
    barkVolume: float,
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
          | _ => `FutureAddedValue(Obj.magic(value): Js.Json.t)
          }: t_dogOrHuman
        );
      },
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
  let makeWithVariables = variables => {
    "query": query,
    "variables": serializeVariables(variables),
    "parse": parse,
  };
  let make = () => {
    "query": query,
    "variables": Js.Json.null,
    "parse": parse,
  };
  let definition = (parse, query, serialize);
};

module UnionExternalFragmentQuery = {
  module DogFragment = {
    let query = "fragment DogFragment on Dog   {\nname  \nbarkVolume  \n}\n";
    module Raw = {
      type t = {
        name: string,
        barkVolume: float,
      };
      type t_Dog = t;
    };
    type t = {
      name: string,
      barkVolume: float,
    };
    type t_Dog = t;

    let parse = (value: Raw.t) => {

      "name": {
        let value = value##name;

        value;
      },

      "barkVolume": {
        let value = value##barkVolume;

        value;
      },
    };
    let serialize: t => Raw.t =
      value => {
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
      };
    let name = "DogFragment";
  };
  module Untitled1 = {
    module Raw = {
      type t = {. "dogOrHuman": t_dogOrHuman}
      and t_dogOrHuman;
    };
    let query =
      (
        (
          "query   {\ndogOrHuman  {\n__typename\n...on Dog   {\n..."
          ++ DogFragment.name
        )
        ++ "   \n}\n\n}\n\n}\n"
      )
      ++ DogFragment.query;
    type t = {. "dogOrHuman": t_dogOrHuman}
    and t_dogOrHuman = [
      | `FutureAddedValue(Js.Json.t)
      | `Dog(DogFragment.t)
    ];
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

                  DogFragment.parse(value);
                },
              )
            | _ => `FutureAddedValue(Obj.magic(value): Js.Json.t)
            }: t_dogOrHuman
          );
        },
      };
    let serialize: t => Raw.t =
      value => {
        let dogOrHuman = {
          let value = value##dogOrHuman;
          switch (value) {
          | `Dog(value) => (
              Obj.magic(DogFragment.serialize(value)): Raw.t_dogOrHuman
            )
          | `FutureAddedValue(value) => (Obj.magic(value): Raw.t_dogOrHuman)
          };
        };
        {

          "dogOrHuman": dogOrHuman,
        };
      };
    let makeWithVariables = variables => {
      "query": query,
      "variables": serializeVariables(variables),
      "parse": parse,
    };
    let make = () => {
      "query": query,
      "variables": Js.Json.null,
      "parse": parse,
    };
    let definition = (parse, query, serialize);
  };
};

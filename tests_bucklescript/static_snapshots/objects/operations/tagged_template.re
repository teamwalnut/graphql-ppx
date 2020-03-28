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
  let query = "query   {\nvariousScalars  {\nnullableString  \nstring  \nnullableInt  \nint  \nnullableFloat  \nfloat  \nnullableBoolean  \nboolean  \nnullableID  \nid  \n}\n\n}\n";
  type raw_t;
  type t = {. "variousScalars": t_variousScalars}
  and t_variousScalars = {
    .
    "nullableString": option(string),
    "string": string,
    "nullableInt": option(int),
    "int": int,
    "nullableFloat": option(float),
    "float": float,
    "nullableBoolean": option(bool),
    "boolean": bool,
    "nullableID": option(string),
    "id": string,
  };
  let parse: Js.Json.t => t =
    value => {
      [@metaloc loc]
      let value = value |> Js.Json.decodeObject |> Js.Option.getExn;
      {

        "variousScalars": {
          let value = Js.Dict.unsafeGet(Obj.magic(value), "variousScalars");
          [@metaloc loc]
          let value = value |> Js.Json.decodeObject |> Js.Option.getExn;
          {

            "nullableString": {
              let value =
                Js.Dict.unsafeGet(Obj.magic(value), "nullableString");

              switch (Js.toOption(Obj.magic(value): Js.Nullable.t('a))) {
              | Some(_) => Some(Obj.magic(value): string)
              | None => None
              };
            },

            "string": {
              let value = Js.Dict.unsafeGet(Obj.magic(value), "string");

              (Obj.magic(value): string);
            },

            "nullableInt": {
              let value = Js.Dict.unsafeGet(Obj.magic(value), "nullableInt");

              switch (Js.toOption(Obj.magic(value): Js.Nullable.t('a))) {
              | Some(_) => Some(Obj.magic(value): int)
              | None => None
              };
            },

            "int": {
              let value = Js.Dict.unsafeGet(Obj.magic(value), "int");

              (Obj.magic(value): int);
            },

            "nullableFloat": {
              let value =
                Js.Dict.unsafeGet(Obj.magic(value), "nullableFloat");

              switch (Js.toOption(Obj.magic(value): Js.Nullable.t('a))) {
              | Some(_) => Some(Obj.magic(value): float)
              | None => None
              };
            },

            "float": {
              let value = Js.Dict.unsafeGet(Obj.magic(value), "float");

              (Obj.magic(value): float);
            },

            "nullableBoolean": {
              let value =
                Js.Dict.unsafeGet(Obj.magic(value), "nullableBoolean");

              switch (Js.toOption(Obj.magic(value): Js.Nullable.t('a))) {
              | Some(_) => Some(Obj.magic(value): bool)
              | None => None
              };
            },

            "boolean": {
              let value = Js.Dict.unsafeGet(Obj.magic(value), "boolean");

              (Obj.magic(value): bool);
            },

            "nullableID": {
              let value = Js.Dict.unsafeGet(Obj.magic(value), "nullableID");

              switch (Js.toOption(Obj.magic(value): Js.Nullable.t('a))) {
              | Some(_) => Some(Obj.magic(value): string)
              | None => None
              };
            },

            "id": {
              let value = Js.Dict.unsafeGet(Obj.magic(value), "id");

              (Obj.magic(value): string);
            },
          };
        },
      };
    };
  let makeVar = (~f, ()) => f(Js.Json.null);
  let definition = (parse, query, makeVar);
};

module MyQuery2 = {
  %raw
  "let { graphql } = require(\"gatsby\")";
  let query = [%raw
    "graphql`\n  query   {\n    variousScalars  {\n      nullableString\n      string\n      nullableInt\n      int\n      nullableFloat\n      float\n      nullableBoolean\n      boolean\n      nullableID\n      id\n    }\n  }\n`"
  ];
  type raw_t;
  type t = {. "variousScalars": t_variousScalars}
  and t_variousScalars = {
    .
    "nullableString": option(string),
    "string": string,
    "nullableInt": option(int),
    "int": int,
    "nullableFloat": option(float),
    "float": float,
    "nullableBoolean": option(bool),
    "boolean": bool,
    "nullableID": option(string),
    "id": string,
  };
  let parse: Js.Json.t => t =
    value => {
      [@metaloc loc]
      let value = value |> Js.Json.decodeObject |> Js.Option.getExn;
      {

        "variousScalars": {
          let value = Js.Dict.unsafeGet(Obj.magic(value), "variousScalars");
          [@metaloc loc]
          let value = value |> Js.Json.decodeObject |> Js.Option.getExn;
          {

            "nullableString": {
              let value =
                Js.Dict.unsafeGet(Obj.magic(value), "nullableString");

              switch (Js.toOption(Obj.magic(value): Js.Nullable.t('a))) {
              | Some(_) => Some(Obj.magic(value): string)
              | None => None
              };
            },

            "string": {
              let value = Js.Dict.unsafeGet(Obj.magic(value), "string");

              (Obj.magic(value): string);
            },

            "nullableInt": {
              let value = Js.Dict.unsafeGet(Obj.magic(value), "nullableInt");

              switch (Js.toOption(Obj.magic(value): Js.Nullable.t('a))) {
              | Some(_) => Some(Obj.magic(value): int)
              | None => None
              };
            },

            "int": {
              let value = Js.Dict.unsafeGet(Obj.magic(value), "int");

              (Obj.magic(value): int);
            },

            "nullableFloat": {
              let value =
                Js.Dict.unsafeGet(Obj.magic(value), "nullableFloat");

              switch (Js.toOption(Obj.magic(value): Js.Nullable.t('a))) {
              | Some(_) => Some(Obj.magic(value): float)
              | None => None
              };
            },

            "float": {
              let value = Js.Dict.unsafeGet(Obj.magic(value), "float");

              (Obj.magic(value): float);
            },

            "nullableBoolean": {
              let value =
                Js.Dict.unsafeGet(Obj.magic(value), "nullableBoolean");

              switch (Js.toOption(Obj.magic(value): Js.Nullable.t('a))) {
              | Some(_) => Some(Obj.magic(value): bool)
              | None => None
              };
            },

            "boolean": {
              let value = Js.Dict.unsafeGet(Obj.magic(value), "boolean");

              (Obj.magic(value): bool);
            },

            "nullableID": {
              let value = Js.Dict.unsafeGet(Obj.magic(value), "nullableID");

              switch (Js.toOption(Obj.magic(value): Js.Nullable.t('a))) {
              | Some(_) => Some(Obj.magic(value): string)
              | None => None
              };
            },

            "id": {
              let value = Js.Dict.unsafeGet(Obj.magic(value), "id");

              (Obj.magic(value): string);
            },
          };
        },
      };
    };
  let makeVar = (~f, ()) => f(Js.Json.null);
  let definition = (parse, query, makeVar);
};

module MyQuery3 = {
  %raw
  "let { graphql } = require(\"gatsby\")";
  let query = [%raw
    "graphql`\n  query   {\n    variousScalars  {\n      nullableString\n      string\n      nullableInt\n      int\n      nullableFloat\n      float\n      nullableBoolean\n      boolean\n      nullableID\n      id\n    }\n  }\n`"
  ];
  type raw_t;
  type t = {. "variousScalars": t_variousScalars}
  and t_variousScalars = {
    .
    "nullableString": option(string),
    "string": string,
    "nullableInt": option(int),
    "int": int,
    "nullableFloat": option(float),
    "float": float,
    "nullableBoolean": option(bool),
    "boolean": bool,
    "nullableID": option(string),
    "id": string,
  };
  let parse: Js.Json.t => t =
    value => {
      [@metaloc loc]
      let value = value |> Js.Json.decodeObject |> Js.Option.getExn;
      {

        "variousScalars": {
          let value = Js.Dict.unsafeGet(Obj.magic(value), "variousScalars");
          [@metaloc loc]
          let value = value |> Js.Json.decodeObject |> Js.Option.getExn;
          {

            "nullableString": {
              let value =
                Js.Dict.unsafeGet(Obj.magic(value), "nullableString");

              switch (Js.toOption(Obj.magic(value): Js.Nullable.t('a))) {
              | Some(_) => Some(Obj.magic(value): string)
              | None => None
              };
            },

            "string": {
              let value = Js.Dict.unsafeGet(Obj.magic(value), "string");

              (Obj.magic(value): string);
            },

            "nullableInt": {
              let value = Js.Dict.unsafeGet(Obj.magic(value), "nullableInt");

              switch (Js.toOption(Obj.magic(value): Js.Nullable.t('a))) {
              | Some(_) => Some(Obj.magic(value): int)
              | None => None
              };
            },

            "int": {
              let value = Js.Dict.unsafeGet(Obj.magic(value), "int");

              (Obj.magic(value): int);
            },

            "nullableFloat": {
              let value =
                Js.Dict.unsafeGet(Obj.magic(value), "nullableFloat");

              switch (Js.toOption(Obj.magic(value): Js.Nullable.t('a))) {
              | Some(_) => Some(Obj.magic(value): float)
              | None => None
              };
            },

            "float": {
              let value = Js.Dict.unsafeGet(Obj.magic(value), "float");

              (Obj.magic(value): float);
            },

            "nullableBoolean": {
              let value =
                Js.Dict.unsafeGet(Obj.magic(value), "nullableBoolean");

              switch (Js.toOption(Obj.magic(value): Js.Nullable.t('a))) {
              | Some(_) => Some(Obj.magic(value): bool)
              | None => None
              };
            },

            "boolean": {
              let value = Js.Dict.unsafeGet(Obj.magic(value), "boolean");

              (Obj.magic(value): bool);
            },

            "nullableID": {
              let value = Js.Dict.unsafeGet(Obj.magic(value), "nullableID");

              switch (Js.toOption(Obj.magic(value): Js.Nullable.t('a))) {
              | Some(_) => Some(Obj.magic(value): string)
              | None => None
              };
            },

            "id": {
              let value = Js.Dict.unsafeGet(Obj.magic(value), "id");

              (Obj.magic(value): string);
            },
          };
        },
      };
    };
  let makeVar = (~f, ()) => f(Js.Json.null);
  let definition = (parse, query, makeVar);
};

module MyQuery4 = {
  %raw
  "let graphql = require(\"gatsby\")";
  let query = [%raw
    "graphql`\n  query   {\n    variousScalars  {\n      nullableString\n      string\n      nullableInt\n      int\n      nullableFloat\n      float\n      nullableBoolean\n      boolean\n      nullableID\n      id\n    }\n  }\n`"
  ];
  type raw_t;
  type t = {. "variousScalars": t_variousScalars}
  and t_variousScalars = {
    .
    "nullableString": option(string),
    "string": string,
    "nullableInt": option(int),
    "int": int,
    "nullableFloat": option(float),
    "float": float,
    "nullableBoolean": option(bool),
    "boolean": bool,
    "nullableID": option(string),
    "id": string,
  };
  let parse: Js.Json.t => t =
    value => {
      [@metaloc loc]
      let value = value |> Js.Json.decodeObject |> Js.Option.getExn;
      {

        "variousScalars": {
          let value = Js.Dict.unsafeGet(Obj.magic(value), "variousScalars");
          [@metaloc loc]
          let value = value |> Js.Json.decodeObject |> Js.Option.getExn;
          {

            "nullableString": {
              let value =
                Js.Dict.unsafeGet(Obj.magic(value), "nullableString");

              switch (Js.toOption(Obj.magic(value): Js.Nullable.t('a))) {
              | Some(_) => Some(Obj.magic(value): string)
              | None => None
              };
            },

            "string": {
              let value = Js.Dict.unsafeGet(Obj.magic(value), "string");

              (Obj.magic(value): string);
            },

            "nullableInt": {
              let value = Js.Dict.unsafeGet(Obj.magic(value), "nullableInt");

              switch (Js.toOption(Obj.magic(value): Js.Nullable.t('a))) {
              | Some(_) => Some(Obj.magic(value): int)
              | None => None
              };
            },

            "int": {
              let value = Js.Dict.unsafeGet(Obj.magic(value), "int");

              (Obj.magic(value): int);
            },

            "nullableFloat": {
              let value =
                Js.Dict.unsafeGet(Obj.magic(value), "nullableFloat");

              switch (Js.toOption(Obj.magic(value): Js.Nullable.t('a))) {
              | Some(_) => Some(Obj.magic(value): float)
              | None => None
              };
            },

            "float": {
              let value = Js.Dict.unsafeGet(Obj.magic(value), "float");

              (Obj.magic(value): float);
            },

            "nullableBoolean": {
              let value =
                Js.Dict.unsafeGet(Obj.magic(value), "nullableBoolean");

              switch (Js.toOption(Obj.magic(value): Js.Nullable.t('a))) {
              | Some(_) => Some(Obj.magic(value): bool)
              | None => None
              };
            },

            "boolean": {
              let value = Js.Dict.unsafeGet(Obj.magic(value), "boolean");

              (Obj.magic(value): bool);
            },

            "nullableID": {
              let value = Js.Dict.unsafeGet(Obj.magic(value), "nullableID");

              switch (Js.toOption(Obj.magic(value): Js.Nullable.t('a))) {
              | Some(_) => Some(Obj.magic(value): string)
              | None => None
              };
            },

            "id": {
              let value = Js.Dict.unsafeGet(Obj.magic(value), "id");

              (Obj.magic(value): string);
            },
          };
        },
      };
    };
  let makeVar = (~f, ()) => f(Js.Json.null);
  let definition = (parse, query, makeVar);
};

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
    type t_variousScalars = {
      .
      "nullableString": Js.Nullable.t(string),
      "string": string,
      "nullableInt": Js.Nullable.t(int),
      "int": int,
      "nullableFloat": Js.Nullable.t(float),
      "float": float,
      "nullableBoolean": Js.Nullable.t(bool),
      "boolean": bool,
      "nullableID": Js.Nullable.t(string),
      "id": string,
    };
    type t = {. "variousScalars": t_variousScalars};
  };
  let query = "query   {\nvariousScalars  {\nnullableString  \nstring  \nnullableInt  \nint  \nnullableFloat  \nfloat  \nnullableBoolean  \nboolean  \nnullableID  \nid  \n}\n\n}\n";
  type t_variousScalars = {
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
  type t = {. "variousScalars": t_variousScalars};
  type operation = t;
  let parse: Raw.t => t =
    value => {
      let variousScalars = {
        let value = value##variousScalars;
        let id = {
          let value = value##id;
          value;
        }
        and nullableID = {
          let value = value##nullableID;
          switch (Js.toOption(value)) {
          | Some(value) => Some(value)
          | None => None
          };
        }
        and boolean = {
          let value = value##boolean;
          value;
        }
        and nullableBoolean = {
          let value = value##nullableBoolean;
          switch (Js.toOption(value)) {
          | Some(value) => Some(value)
          | None => None
          };
        }
        and float = {
          let value = value##float;
          value;
        }
        and nullableFloat = {
          let value = value##nullableFloat;
          switch (Js.toOption(value)) {
          | Some(value) => Some(value)
          | None => None
          };
        }
        and int = {
          let value = value##int;
          value;
        }
        and nullableInt = {
          let value = value##nullableInt;
          switch (Js.toOption(value)) {
          | Some(value) => Some(value)
          | None => None
          };
        }
        and string = {
          let value = value##string;
          value;
        }
        and nullableString = {
          let value = value##nullableString;
          switch (Js.toOption(value)) {
          | Some(value) => Some(value)
          | None => None
          };
        };
        {
          "nullableString": nullableString,
          "string": string,
          "nullableInt": nullableInt,
          "int": int,
          "nullableFloat": nullableFloat,
          "float": float,
          "nullableBoolean": nullableBoolean,
          "boolean": boolean,
          "nullableID": nullableID,
          "id": id,
        };
      };
      {"variousScalars": variousScalars};
    };
  let serialize: t => Raw.t =
    value => {
      let variousScalars = {
        let value = value##variousScalars;
        let id = {
          let value = value##id;
          value;
        }
        and nullableID = {
          let value = value##nullableID;
          switch (value) {
          | Some(value) => Js.Nullable.return(value)
          | None => Js.Nullable.null
          };
        }
        and boolean = {
          let value = value##boolean;
          value;
        }
        and nullableBoolean = {
          let value = value##nullableBoolean;
          switch (value) {
          | Some(value) => Js.Nullable.return(value)
          | None => Js.Nullable.null
          };
        }
        and float = {
          let value = value##float;
          value;
        }
        and nullableFloat = {
          let value = value##nullableFloat;
          switch (value) {
          | Some(value) => Js.Nullable.return(value)
          | None => Js.Nullable.null
          };
        }
        and int = {
          let value = value##int;
          value;
        }
        and nullableInt = {
          let value = value##nullableInt;
          switch (value) {
          | Some(value) => Js.Nullable.return(value)
          | None => Js.Nullable.null
          };
        }
        and string = {
          let value = value##string;
          value;
        }
        and nullableString = {
          let value = value##nullableString;
          switch (value) {
          | Some(value) => Js.Nullable.return(value)
          | None => Js.Nullable.null
          };
        };
        {
          "nullableString": nullableString,
          "string": string,
          "nullableInt": nullableInt,
          "int": int,
          "nullableFloat": nullableFloat,
          "float": float,
          "nullableBoolean": nullableBoolean,
          "boolean": boolean,
          "nullableID": nullableID,
          "id": id,
        };
      };
      {"variousScalars": variousScalars};
    };
  let definition = (parse, query, serialize);
};

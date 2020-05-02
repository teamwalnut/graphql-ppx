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
      __typename: string,
      nullableString: Js.Nullable.t(string),
      string,
      nullableInt: Js.Nullable.t(int),
      int,
      nullableFloat: Js.Nullable.t(float),
      float,
      nullableBoolean: Js.Nullable.t(bool),
      boolean: bool,
      nullableID: Js.Nullable.t(string),
      id: string,
    };
    type t = {variousScalars: t_variousScalars};
  };
  let query = "query   {\nvariousScalars  {\n__typename  \nnullableString  \nstring  \nnullableInt  \nint  \nnullableFloat  \nfloat  \nnullableBoolean  \nboolean  \nnullableID  \nid  \n}\n\n}\n";
  type t_variousScalars = {
    __typename: string,
    nullableString: option(string),
    string,
    nullableInt: option(int),
    int,
    nullableFloat: option(float),
    float,
    nullableBoolean: option(bool),
    boolean: bool,
    nullableID: option(string),
    id: string,
  };
  type t = {variousScalars: t_variousScalars};
  let parse: Raw.t => t =
    (value) => (
      {
        let variousScalars = {
          let value = (value: Raw.t).variousScalars;
          (
            {
              let id = {
                let value = (value: Raw.t_variousScalars).id;
                value;
              }
              and nullableID = {
                let value = (value: Raw.t_variousScalars).nullableID;
                switch (Js.toOption(value)) {
                | Some(value) => Some(value)
                | None => None
                };
              }
              and boolean = {
                let value = (value: Raw.t_variousScalars).boolean;
                value;
              }
              and nullableBoolean = {
                let value = (value: Raw.t_variousScalars).nullableBoolean;
                switch (Js.toOption(value)) {
                | Some(value) => Some(value)
                | None => None
                };
              }
              and float = {
                let value = (value: Raw.t_variousScalars).float;
                value;
              }
              and nullableFloat = {
                let value = (value: Raw.t_variousScalars).nullableFloat;
                switch (Js.toOption(value)) {
                | Some(value) => Some(value)
                | None => None
                };
              }
              and int = {
                let value = (value: Raw.t_variousScalars).int;
                value;
              }
              and nullableInt = {
                let value = (value: Raw.t_variousScalars).nullableInt;
                switch (Js.toOption(value)) {
                | Some(value) => Some(value)
                | None => None
                };
              }
              and string = {
                let value = (value: Raw.t_variousScalars).string;
                value;
              }
              and nullableString = {
                let value = (value: Raw.t_variousScalars).nullableString;
                switch (Js.toOption(value)) {
                | Some(value) => Some(value)
                | None => None
                };
              }
              and __typename = {
                let value = (value: Raw.t_variousScalars).__typename;
                value;
              };
              {

                __typename,

                nullableString,

                string,

                nullableInt,

                int,

                nullableFloat,

                float,

                nullableBoolean,

                boolean,

                nullableID,

                id,
              };
            }: t_variousScalars
          );
        };
        {

          variousScalars: variousScalars,
        };
      }: t
    );
  let serialize: t => Raw.t =
    (value) => (
      {
        let variousScalars = {
          let value = (value: t).variousScalars;
          (
            {
              let id = {
                let value = (value: t_variousScalars).id;

                value;
              }
              and nullableID = {
                let value = (value: t_variousScalars).nullableID;

                switch (value) {
                | Some(value) => Js.Nullable.return(value)
                | None => Js.Nullable.null
                };
              }
              and boolean = {
                let value = (value: t_variousScalars).boolean;

                value;
              }
              and nullableBoolean = {
                let value = (value: t_variousScalars).nullableBoolean;

                switch (value) {
                | Some(value) => Js.Nullable.return(value)
                | None => Js.Nullable.null
                };
              }
              and float = {
                let value = (value: t_variousScalars).float;

                value;
              }
              and nullableFloat = {
                let value = (value: t_variousScalars).nullableFloat;

                switch (value) {
                | Some(value) => Js.Nullable.return(value)
                | None => Js.Nullable.null
                };
              }
              and int = {
                let value = (value: t_variousScalars).int;

                value;
              }
              and nullableInt = {
                let value = (value: t_variousScalars).nullableInt;

                switch (value) {
                | Some(value) => Js.Nullable.return(value)
                | None => Js.Nullable.null
                };
              }
              and string = {
                let value = (value: t_variousScalars).string;

                value;
              }
              and nullableString = {
                let value = (value: t_variousScalars).nullableString;

                switch (value) {
                | Some(value) => Js.Nullable.return(value)
                | None => Js.Nullable.null
                };
              }
              and __typename = {
                let value = (value: t_variousScalars).__typename;

                value;
              };
              {

                __typename,

                nullableString,

                string,

                nullableInt,

                int,

                nullableFloat,

                float,

                nullableBoolean,

                boolean,

                nullableID,

                id,
              };
            }: Raw.t_variousScalars
          );
        };
        {

          variousScalars: variousScalars,
        };
      }: Raw.t
    );
  let definition = (parse, query, serialize);
};

module MyQuery2 = {
  module Raw = {
    type t_variousScalars = {
      __typename: string,
      nullableString: Js.Nullable.t(string),
      string,
      nullableInt: Js.Nullable.t(int),
      int,
      nullableFloat: Js.Nullable.t(float),
      float,
      nullableBoolean: Js.Nullable.t(bool),
      boolean: bool,
      nullableID: Js.Nullable.t(string),
      id: string,
    };
    type t = {variousScalars: t_variousScalars};
  };
  %raw
  "let { graphql } = require(\"gatsby\")";
  let query = [%raw
    "graphql`\n  query   {\n    variousScalars  {\n      __typename\n      nullableString\n      string\n      nullableInt\n      int\n      nullableFloat\n      float\n      nullableBoolean\n      boolean\n      nullableID\n      id\n    }\n  }\n`"
  ];
  type t_variousScalars = {
    __typename: string,
    nullableString: option(string),
    string,
    nullableInt: option(int),
    int,
    nullableFloat: option(float),
    float,
    nullableBoolean: option(bool),
    boolean: bool,
    nullableID: option(string),
    id: string,
  };
  type t = {variousScalars: t_variousScalars};
  let parse: Raw.t => t =
    (value) => (
      {
        let variousScalars = {
          let value = (value: Raw.t).variousScalars;
          (
            {
              let id = {
                let value = (value: Raw.t_variousScalars).id;
                value;
              }
              and nullableID = {
                let value = (value: Raw.t_variousScalars).nullableID;
                switch (Js.toOption(value)) {
                | Some(value) => Some(value)
                | None => None
                };
              }
              and boolean = {
                let value = (value: Raw.t_variousScalars).boolean;
                value;
              }
              and nullableBoolean = {
                let value = (value: Raw.t_variousScalars).nullableBoolean;
                switch (Js.toOption(value)) {
                | Some(value) => Some(value)
                | None => None
                };
              }
              and float = {
                let value = (value: Raw.t_variousScalars).float;
                value;
              }
              and nullableFloat = {
                let value = (value: Raw.t_variousScalars).nullableFloat;
                switch (Js.toOption(value)) {
                | Some(value) => Some(value)
                | None => None
                };
              }
              and int = {
                let value = (value: Raw.t_variousScalars).int;
                value;
              }
              and nullableInt = {
                let value = (value: Raw.t_variousScalars).nullableInt;
                switch (Js.toOption(value)) {
                | Some(value) => Some(value)
                | None => None
                };
              }
              and string = {
                let value = (value: Raw.t_variousScalars).string;
                value;
              }
              and nullableString = {
                let value = (value: Raw.t_variousScalars).nullableString;
                switch (Js.toOption(value)) {
                | Some(value) => Some(value)
                | None => None
                };
              }
              and __typename = {
                let value = (value: Raw.t_variousScalars).__typename;
                value;
              };
              {

                __typename,

                nullableString,

                string,

                nullableInt,

                int,

                nullableFloat,

                float,

                nullableBoolean,

                boolean,

                nullableID,

                id,
              };
            }: t_variousScalars
          );
        };
        {

          variousScalars: variousScalars,
        };
      }: t
    );
  let serialize: t => Raw.t =
    (value) => (
      {
        let variousScalars = {
          let value = (value: t).variousScalars;
          (
            {
              let id = {
                let value = (value: t_variousScalars).id;

                value;
              }
              and nullableID = {
                let value = (value: t_variousScalars).nullableID;

                switch (value) {
                | Some(value) => Js.Nullable.return(value)
                | None => Js.Nullable.null
                };
              }
              and boolean = {
                let value = (value: t_variousScalars).boolean;

                value;
              }
              and nullableBoolean = {
                let value = (value: t_variousScalars).nullableBoolean;

                switch (value) {
                | Some(value) => Js.Nullable.return(value)
                | None => Js.Nullable.null
                };
              }
              and float = {
                let value = (value: t_variousScalars).float;

                value;
              }
              and nullableFloat = {
                let value = (value: t_variousScalars).nullableFloat;

                switch (value) {
                | Some(value) => Js.Nullable.return(value)
                | None => Js.Nullable.null
                };
              }
              and int = {
                let value = (value: t_variousScalars).int;

                value;
              }
              and nullableInt = {
                let value = (value: t_variousScalars).nullableInt;

                switch (value) {
                | Some(value) => Js.Nullable.return(value)
                | None => Js.Nullable.null
                };
              }
              and string = {
                let value = (value: t_variousScalars).string;

                value;
              }
              and nullableString = {
                let value = (value: t_variousScalars).nullableString;

                switch (value) {
                | Some(value) => Js.Nullable.return(value)
                | None => Js.Nullable.null
                };
              }
              and __typename = {
                let value = (value: t_variousScalars).__typename;

                value;
              };
              {

                __typename,

                nullableString,

                string,

                nullableInt,

                int,

                nullableFloat,

                float,

                nullableBoolean,

                boolean,

                nullableID,

                id,
              };
            }: Raw.t_variousScalars
          );
        };
        {

          variousScalars: variousScalars,
        };
      }: Raw.t
    );
  let definition = (parse, query, serialize);
};

module MyQuery3 = {
  module Raw = {
    type t_variousScalars = {
      __typename: string,
      nullableString: Js.Nullable.t(string),
      string,
      nullableInt: Js.Nullable.t(int),
      int,
      nullableFloat: Js.Nullable.t(float),
      float,
      nullableBoolean: Js.Nullable.t(bool),
      boolean: bool,
      nullableID: Js.Nullable.t(string),
      id: string,
    };
    type t = {variousScalars: t_variousScalars};
  };
  %raw
  "let { graphql } = require(\"gatsby\")";
  let query = [%raw
    "graphql`\n  query   {\n    variousScalars  {\n      __typename\n      nullableString\n      string\n      nullableInt\n      int\n      nullableFloat\n      float\n      nullableBoolean\n      boolean\n      nullableID\n      id\n    }\n  }\n`"
  ];
  type t_variousScalars = {
    __typename: string,
    nullableString: option(string),
    string,
    nullableInt: option(int),
    int,
    nullableFloat: option(float),
    float,
    nullableBoolean: option(bool),
    boolean: bool,
    nullableID: option(string),
    id: string,
  };
  type t = {variousScalars: t_variousScalars};
  let parse: Raw.t => t =
    (value) => (
      {
        let variousScalars = {
          let value = (value: Raw.t).variousScalars;
          (
            {
              let id = {
                let value = (value: Raw.t_variousScalars).id;
                value;
              }
              and nullableID = {
                let value = (value: Raw.t_variousScalars).nullableID;
                switch (Js.toOption(value)) {
                | Some(value) => Some(value)
                | None => None
                };
              }
              and boolean = {
                let value = (value: Raw.t_variousScalars).boolean;
                value;
              }
              and nullableBoolean = {
                let value = (value: Raw.t_variousScalars).nullableBoolean;
                switch (Js.toOption(value)) {
                | Some(value) => Some(value)
                | None => None
                };
              }
              and float = {
                let value = (value: Raw.t_variousScalars).float;
                value;
              }
              and nullableFloat = {
                let value = (value: Raw.t_variousScalars).nullableFloat;
                switch (Js.toOption(value)) {
                | Some(value) => Some(value)
                | None => None
                };
              }
              and int = {
                let value = (value: Raw.t_variousScalars).int;
                value;
              }
              and nullableInt = {
                let value = (value: Raw.t_variousScalars).nullableInt;
                switch (Js.toOption(value)) {
                | Some(value) => Some(value)
                | None => None
                };
              }
              and string = {
                let value = (value: Raw.t_variousScalars).string;
                value;
              }
              and nullableString = {
                let value = (value: Raw.t_variousScalars).nullableString;
                switch (Js.toOption(value)) {
                | Some(value) => Some(value)
                | None => None
                };
              }
              and __typename = {
                let value = (value: Raw.t_variousScalars).__typename;
                value;
              };
              {

                __typename,

                nullableString,

                string,

                nullableInt,

                int,

                nullableFloat,

                float,

                nullableBoolean,

                boolean,

                nullableID,

                id,
              };
            }: t_variousScalars
          );
        };
        {

          variousScalars: variousScalars,
        };
      }: t
    );
  let serialize: t => Raw.t =
    (value) => (
      {
        let variousScalars = {
          let value = (value: t).variousScalars;
          (
            {
              let id = {
                let value = (value: t_variousScalars).id;

                value;
              }
              and nullableID = {
                let value = (value: t_variousScalars).nullableID;

                switch (value) {
                | Some(value) => Js.Nullable.return(value)
                | None => Js.Nullable.null
                };
              }
              and boolean = {
                let value = (value: t_variousScalars).boolean;

                value;
              }
              and nullableBoolean = {
                let value = (value: t_variousScalars).nullableBoolean;

                switch (value) {
                | Some(value) => Js.Nullable.return(value)
                | None => Js.Nullable.null
                };
              }
              and float = {
                let value = (value: t_variousScalars).float;

                value;
              }
              and nullableFloat = {
                let value = (value: t_variousScalars).nullableFloat;

                switch (value) {
                | Some(value) => Js.Nullable.return(value)
                | None => Js.Nullable.null
                };
              }
              and int = {
                let value = (value: t_variousScalars).int;

                value;
              }
              and nullableInt = {
                let value = (value: t_variousScalars).nullableInt;

                switch (value) {
                | Some(value) => Js.Nullable.return(value)
                | None => Js.Nullable.null
                };
              }
              and string = {
                let value = (value: t_variousScalars).string;

                value;
              }
              and nullableString = {
                let value = (value: t_variousScalars).nullableString;

                switch (value) {
                | Some(value) => Js.Nullable.return(value)
                | None => Js.Nullable.null
                };
              }
              and __typename = {
                let value = (value: t_variousScalars).__typename;

                value;
              };
              {

                __typename,

                nullableString,

                string,

                nullableInt,

                int,

                nullableFloat,

                float,

                nullableBoolean,

                boolean,

                nullableID,

                id,
              };
            }: Raw.t_variousScalars
          );
        };
        {

          variousScalars: variousScalars,
        };
      }: Raw.t
    );
  let definition = (parse, query, serialize);
};

module MyQuery4 = {
  module Raw = {
    type t_variousScalars = {
      __typename: string,
      nullableString: Js.Nullable.t(string),
      string,
      nullableInt: Js.Nullable.t(int),
      int,
      nullableFloat: Js.Nullable.t(float),
      float,
      nullableBoolean: Js.Nullable.t(bool),
      boolean: bool,
      nullableID: Js.Nullable.t(string),
      id: string,
    };
    type t = {variousScalars: t_variousScalars};
  };
  %raw
  "let graphql = require(\"gatsby\")";
  let query = [%raw
    "graphql`\n  query   {\n    variousScalars  {\n      __typename\n      nullableString\n      string\n      nullableInt\n      int\n      nullableFloat\n      float\n      nullableBoolean\n      boolean\n      nullableID\n      id\n    }\n  }\n`"
  ];
  type t_variousScalars = {
    __typename: string,
    nullableString: option(string),
    string,
    nullableInt: option(int),
    int,
    nullableFloat: option(float),
    float,
    nullableBoolean: option(bool),
    boolean: bool,
    nullableID: option(string),
    id: string,
  };
  type t = {variousScalars: t_variousScalars};
  let parse: Raw.t => t =
    (value) => (
      {
        let variousScalars = {
          let value = (value: Raw.t).variousScalars;
          (
            {
              let id = {
                let value = (value: Raw.t_variousScalars).id;
                value;
              }
              and nullableID = {
                let value = (value: Raw.t_variousScalars).nullableID;
                switch (Js.toOption(value)) {
                | Some(value) => Some(value)
                | None => None
                };
              }
              and boolean = {
                let value = (value: Raw.t_variousScalars).boolean;
                value;
              }
              and nullableBoolean = {
                let value = (value: Raw.t_variousScalars).nullableBoolean;
                switch (Js.toOption(value)) {
                | Some(value) => Some(value)
                | None => None
                };
              }
              and float = {
                let value = (value: Raw.t_variousScalars).float;
                value;
              }
              and nullableFloat = {
                let value = (value: Raw.t_variousScalars).nullableFloat;
                switch (Js.toOption(value)) {
                | Some(value) => Some(value)
                | None => None
                };
              }
              and int = {
                let value = (value: Raw.t_variousScalars).int;
                value;
              }
              and nullableInt = {
                let value = (value: Raw.t_variousScalars).nullableInt;
                switch (Js.toOption(value)) {
                | Some(value) => Some(value)
                | None => None
                };
              }
              and string = {
                let value = (value: Raw.t_variousScalars).string;
                value;
              }
              and nullableString = {
                let value = (value: Raw.t_variousScalars).nullableString;
                switch (Js.toOption(value)) {
                | Some(value) => Some(value)
                | None => None
                };
              }
              and __typename = {
                let value = (value: Raw.t_variousScalars).__typename;
                value;
              };
              {

                __typename,

                nullableString,

                string,

                nullableInt,

                int,

                nullableFloat,

                float,

                nullableBoolean,

                boolean,

                nullableID,

                id,
              };
            }: t_variousScalars
          );
        };
        {

          variousScalars: variousScalars,
        };
      }: t
    );
  let serialize: t => Raw.t =
    (value) => (
      {
        let variousScalars = {
          let value = (value: t).variousScalars;
          (
            {
              let id = {
                let value = (value: t_variousScalars).id;

                value;
              }
              and nullableID = {
                let value = (value: t_variousScalars).nullableID;

                switch (value) {
                | Some(value) => Js.Nullable.return(value)
                | None => Js.Nullable.null
                };
              }
              and boolean = {
                let value = (value: t_variousScalars).boolean;

                value;
              }
              and nullableBoolean = {
                let value = (value: t_variousScalars).nullableBoolean;

                switch (value) {
                | Some(value) => Js.Nullable.return(value)
                | None => Js.Nullable.null
                };
              }
              and float = {
                let value = (value: t_variousScalars).float;

                value;
              }
              and nullableFloat = {
                let value = (value: t_variousScalars).nullableFloat;

                switch (value) {
                | Some(value) => Js.Nullable.return(value)
                | None => Js.Nullable.null
                };
              }
              and int = {
                let value = (value: t_variousScalars).int;

                value;
              }
              and nullableInt = {
                let value = (value: t_variousScalars).nullableInt;

                switch (value) {
                | Some(value) => Js.Nullable.return(value)
                | None => Js.Nullable.null
                };
              }
              and string = {
                let value = (value: t_variousScalars).string;

                value;
              }
              and nullableString = {
                let value = (value: t_variousScalars).nullableString;

                switch (value) {
                | Some(value) => Js.Nullable.return(value)
                | None => Js.Nullable.null
                };
              }
              and __typename = {
                let value = (value: t_variousScalars).__typename;

                value;
              };
              {

                __typename,

                nullableString,

                string,

                nullableInt,

                int,

                nullableFloat,

                float,

                nullableBoolean,

                boolean,

                nullableID,

                id,
              };
            }: Raw.t_variousScalars
          );
        };
        {

          variousScalars: variousScalars,
        };
      }: Raw.t
    );
  let definition = (parse, query, serialize);
};

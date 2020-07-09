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
module GraphQL_PPX = {
  let deepMerge = (json1, _) => json1;
};
module MyQuery = {
  [@ocaml.warning "-32"];
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
    type t_variables = unit;
  };
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
  /**The GraphQL query string*/
  let query: string = [%raw
    "graphql`\n  query   {\n    variousScalars  {\n      nullableString\n      string\n      nullableInt\n      int\n      nullableFloat\n      float\n      nullableBoolean\n      boolean\n      nullableID\n      id\n    }\n  }\n`"
  ];
  type t_variables = unit;
  /**Parse the JSON GraphQL data to ReasonML data types*/
  let parse = (value: Raw.t): t => {
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
  /**Serialize the ReasonML GraphQL data that was parsed using the parse function back to the original JSON compatible data */
  let serialize = (value: t): Raw.t => {
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
  let serializeVariables = () => ();
  let makeVariables = () => ();
  let makeDefaultVariables = () => makeVariables();
  external unsafe_fromJson: Js.Json.t => Raw.t = "%identity";
  external toJson: Raw.t => Js.Json.t = "%identity";
  external variablesToJson: Raw.t_variables => Js.Json.t = "%identity";
  module Z__INTERNAL = {
    type root = t;
    type nonrec graphql_module;
    /****--- graphql-ppx module ---**

The contents of this module are automatically generated by `graphql-ppx`.
The following is simply an overview of the most important variables and types that you can access from this module.

```
module MyQuery {
  /**
  The GraphQL query string
  */
  let query: string;

  /**
  This is the main type of the result you will get back.
  You can hover above the identifier key (e.g. query or mutation) to see the fully generated type for your module.
  */
  type t;

  /**
  Parse the JSON GraphQL data to ReasonML data types
  */
  let parse: Raw.t => t;

  /**
  Serialize the ReasonML GraphQL data that was parsed using the parse function back to the original JSON compatible data
  */
  let serialize: t => Raw.t;

  /**
  This is the JSON compatible type of the GraphQL data.
  It should not be necessary to access the types inside for normal use cases.
  */
  module Raw: { type t; };
}
```*/
    let graphql_module: graphql_module = Obj.magic(0);
  };
};

module MyTypes = {
  type query;
};
module MyQuery1a = {
  [@ocaml.warning "-32"];
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
    type t_variables = unit;
  };
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
  /**The GraphQL query string*/
  let query: MyTypes.query = [%raw
    "graphql`\n  query   {\n    variousScalars  {\n      nullableString\n      string\n      nullableInt\n      int\n      nullableFloat\n      float\n      nullableBoolean\n      boolean\n      nullableID\n      id\n    }\n  }\n`"
  ];
  type t_variables = unit;
  /**Parse the JSON GraphQL data to ReasonML data types*/
  let parse = (value: Raw.t): t => {
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
  /**Serialize the ReasonML GraphQL data that was parsed using the parse function back to the original JSON compatible data */
  let serialize = (value: t): Raw.t => {
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
  let serializeVariables = () => ();
  let makeVariables = () => ();
  let makeDefaultVariables = () => makeVariables();
  external unsafe_fromJson: Js.Json.t => Raw.t = "%identity";
  external toJson: Raw.t => Js.Json.t = "%identity";
  external variablesToJson: Raw.t_variables => Js.Json.t = "%identity";
  module Z__INTERNAL = {
    type root = t;
    type nonrec graphql_module;
    /****--- graphql-ppx module ---**

The contents of this module are automatically generated by `graphql-ppx`.
The following is simply an overview of the most important variables and types that you can access from this module.

```
module MyQuery1a {
  /**
  The GraphQL query string
  */
  let query: string;

  /**
  This is the main type of the result you will get back.
  You can hover above the identifier key (e.g. query or mutation) to see the fully generated type for your module.
  */
  type t;

  /**
  Parse the JSON GraphQL data to ReasonML data types
  */
  let parse: Raw.t => t;

  /**
  Serialize the ReasonML GraphQL data that was parsed using the parse function back to the original JSON compatible data
  */
  let serialize: t => Raw.t;

  /**
  This is the JSON compatible type of the GraphQL data.
  It should not be necessary to access the types inside for normal use cases.
  */
  module Raw: { type t; };
}
```*/
    let graphql_module: graphql_module = Obj.magic(0);
  };
};
module MyQuery1b = {
  [@ocaml.warning "-32"];
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
    type t_variables = unit;
  };
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
  /**The GraphQL query string*/
  let query: string = [%raw
    "graphql`\n  query   {\n    variousScalars  {\n      nullableString\n      string\n      nullableInt\n      int\n      nullableFloat\n      float\n      nullableBoolean\n      boolean\n      nullableID\n      id\n    }\n  }\n`"
  ];
  type t_variables = unit;
  /**Parse the JSON GraphQL data to ReasonML data types*/
  let parse = (value: Raw.t): t => {
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
  /**Serialize the ReasonML GraphQL data that was parsed using the parse function back to the original JSON compatible data */
  let serialize = (value: t): Raw.t => {
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
  let serializeVariables = () => ();
  let makeVariables = () => ();
  let makeDefaultVariables = () => makeVariables();
  external unsafe_fromJson: Js.Json.t => Raw.t = "%identity";
  external toJson: Raw.t => Js.Json.t = "%identity";
  external variablesToJson: Raw.t_variables => Js.Json.t = "%identity";
  module Z__INTERNAL = {
    type root = t;
    type nonrec graphql_module;
    /****--- graphql-ppx module ---**

The contents of this module are automatically generated by `graphql-ppx`.
The following is simply an overview of the most important variables and types that you can access from this module.

```
module MyQuery1b {
  /**
  The GraphQL query string
  */
  let query: string;

  /**
  This is the main type of the result you will get back.
  You can hover above the identifier key (e.g. query or mutation) to see the fully generated type for your module.
  */
  type t;

  /**
  Parse the JSON GraphQL data to ReasonML data types
  */
  let parse: Raw.t => t;

  /**
  Serialize the ReasonML GraphQL data that was parsed using the parse function back to the original JSON compatible data
  */
  let serialize: t => Raw.t;

  /**
  This is the JSON compatible type of the GraphQL data.
  It should not be necessary to access the types inside for normal use cases.
  */
  module Raw: { type t; };
}
```*/
    let graphql_module: graphql_module = Obj.magic(0);
  };
};
module MyQuery2 = {
  [@ocaml.warning "-32"];
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
    type t_variables = unit;
  };
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
  /**The GraphQL query string*/
  let query: string = [%raw
    "require(\"gatsby\").graphql`\n  query   {\n    variousScalars  {\n      nullableString\n      string\n      nullableInt\n      int\n      nullableFloat\n      float\n      nullableBoolean\n      boolean\n      nullableID\n      id\n    }\n  }\n`"
  ];
  type t_variables = unit;
  /**Parse the JSON GraphQL data to ReasonML data types*/
  let parse = (value: Raw.t): t => {
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
  /**Serialize the ReasonML GraphQL data that was parsed using the parse function back to the original JSON compatible data */
  let serialize = (value: t): Raw.t => {
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
  let serializeVariables = () => ();
  let makeVariables = () => ();
  let makeDefaultVariables = () => makeVariables();
  external unsafe_fromJson: Js.Json.t => Raw.t = "%identity";
  external toJson: Raw.t => Js.Json.t = "%identity";
  external variablesToJson: Raw.t_variables => Js.Json.t = "%identity";
  module Z__INTERNAL = {
    type root = t;
    type nonrec graphql_module;
    /****--- graphql-ppx module ---**

The contents of this module are automatically generated by `graphql-ppx`.
The following is simply an overview of the most important variables and types that you can access from this module.

```
module MyQuery2 {
  /**
  The GraphQL query string
  */
  let query: string;

  /**
  This is the main type of the result you will get back.
  You can hover above the identifier key (e.g. query or mutation) to see the fully generated type for your module.
  */
  type t;

  /**
  Parse the JSON GraphQL data to ReasonML data types
  */
  let parse: Raw.t => t;

  /**
  Serialize the ReasonML GraphQL data that was parsed using the parse function back to the original JSON compatible data
  */
  let serialize: t => Raw.t;

  /**
  This is the JSON compatible type of the GraphQL data.
  It should not be necessary to access the types inside for normal use cases.
  */
  module Raw: { type t; };
}
```*/
    let graphql_module: graphql_module = Obj.magic(0);
  };
};
module MyQuery3 = {
  [@ocaml.warning "-32"];
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
    type t_variables = unit;
  };
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
  /**The GraphQL query string*/
  let query: string = [%raw
    "require(\"gatsby\").graphql`\n  query   {\n    variousScalars  {\n      nullableString\n      string\n      nullableInt\n      int\n      nullableFloat\n      float\n      nullableBoolean\n      boolean\n      nullableID\n      id\n    }\n  }\n`"
  ];
  type t_variables = unit;
  /**Parse the JSON GraphQL data to ReasonML data types*/
  let parse = (value: Raw.t): t => {
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
  /**Serialize the ReasonML GraphQL data that was parsed using the parse function back to the original JSON compatible data */
  let serialize = (value: t): Raw.t => {
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
  let serializeVariables = () => ();
  let makeVariables = () => ();
  let makeDefaultVariables = () => makeVariables();
  external unsafe_fromJson: Js.Json.t => Raw.t = "%identity";
  external toJson: Raw.t => Js.Json.t = "%identity";
  external variablesToJson: Raw.t_variables => Js.Json.t = "%identity";
  module Z__INTERNAL = {
    type root = t;
    type nonrec graphql_module;
    /****--- graphql-ppx module ---**

The contents of this module are automatically generated by `graphql-ppx`.
The following is simply an overview of the most important variables and types that you can access from this module.

```
module MyQuery3 {
  /**
  The GraphQL query string
  */
  let query: string;

  /**
  This is the main type of the result you will get back.
  You can hover above the identifier key (e.g. query or mutation) to see the fully generated type for your module.
  */
  type t;

  /**
  Parse the JSON GraphQL data to ReasonML data types
  */
  let parse: Raw.t => t;

  /**
  Serialize the ReasonML GraphQL data that was parsed using the parse function back to the original JSON compatible data
  */
  let serialize: t => Raw.t;

  /**
  This is the JSON compatible type of the GraphQL data.
  It should not be necessary to access the types inside for normal use cases.
  */
  module Raw: { type t; };
}
```*/
    let graphql_module: graphql_module = Obj.magic(0);
  };
};
module MyQuery4 = {
  [@ocaml.warning "-32"];
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
    type t_variables = unit;
  };
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
  /**The GraphQL query string*/
  let query: string = [%raw
    "require(\"gatsby\")`\n  query   {\n    variousScalars  {\n      nullableString\n      string\n      nullableInt\n      int\n      nullableFloat\n      float\n      nullableBoolean\n      boolean\n      nullableID\n      id\n    }\n  }\n`"
  ];
  type t_variables = unit;
  /**Parse the JSON GraphQL data to ReasonML data types*/
  let parse = (value: Raw.t): t => {
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
  /**Serialize the ReasonML GraphQL data that was parsed using the parse function back to the original JSON compatible data */
  let serialize = (value: t): Raw.t => {
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
  let serializeVariables = () => ();
  let makeVariables = () => ();
  let makeDefaultVariables = () => makeVariables();
  external unsafe_fromJson: Js.Json.t => Raw.t = "%identity";
  external toJson: Raw.t => Js.Json.t = "%identity";
  external variablesToJson: Raw.t_variables => Js.Json.t = "%identity";
  module Z__INTERNAL = {
    type root = t;
    type nonrec graphql_module;
    /****--- graphql-ppx module ---**

The contents of this module are automatically generated by `graphql-ppx`.
The following is simply an overview of the most important variables and types that you can access from this module.

```
module MyQuery4 {
  /**
  The GraphQL query string
  */
  let query: string;

  /**
  This is the main type of the result you will get back.
  You can hover above the identifier key (e.g. query or mutation) to see the fully generated type for your module.
  */
  type t;

  /**
  Parse the JSON GraphQL data to ReasonML data types
  */
  let parse: Raw.t => t;

  /**
  Serialize the ReasonML GraphQL data that was parsed using the parse function back to the original JSON compatible data
  */
  let serialize: t => Raw.t;

  /**
  This is the JSON compatible type of the GraphQL data.
  It should not be necessary to access the types inside for normal use cases.
  */
  module Raw: { type t; };
}
```*/
    let graphql_module: graphql_module = Obj.magic(0);
  };
};
module ListFragment = {
  [@ocaml.warning "-32"];
  module Raw = {
    type t = {
      .
      "nullableOfNullable": Js.Nullable.t(array(Js.Nullable.t(string))),
      "nullableOfNonNullable": Js.Nullable.t(array(string)),
    };
    type nonrec t_Lists = t;
  };
  type t = {
    .
    "nullableOfNullable": option(array(option(string))),
    "nullableOfNonNullable": option(array(string)),
  };
  type nonrec t_Lists = t;
  /**The GraphQL query string*/
  let query: string = [%raw
    "require(\"gatsby\")`\n  fragment ListFragment on Lists   {\n    nullableOfNullable\n    nullableOfNonNullable\n  }\n`"
  ];
  /**Parse the JSON GraphQL data to ReasonML data types*/
  let parse = (value: Raw.t): t => {
    let nullableOfNonNullable = {
      let value = value##nullableOfNonNullable;
      switch (Js.toOption(value)) {
      | Some(value) => Some(value |> Js.Array.map(value => value))
      | None => None
      };
    }
    and nullableOfNullable = {
      let value = value##nullableOfNullable;
      switch (Js.toOption(value)) {
      | Some(value) =>
        Some(
          value
          |> Js.Array.map(value =>
               switch (Js.toOption(value)) {
               | Some(value) => Some(value)
               | None => None
               }
             ),
        )
      | None => None
      };
    };
    {
      "nullableOfNullable": nullableOfNullable,
      "nullableOfNonNullable": nullableOfNonNullable,
    };
  };

  let verifyArgsAndParse =
      (~fragmentName as _ListFragment: [ | `ListFragment], value: Raw.t) =>
    parse(value);
  /**Serialize the ReasonML GraphQL data that was parsed using the parse function back to the original JSON compatible data */
  let serialize = (value: t): Raw.t => {
    let nullableOfNonNullable = {
      let value = value##nullableOfNonNullable;
      switch (value) {
      | Some(value) =>
        Js.Nullable.return(value |> Js.Array.map(value => value))
      | None => Js.Nullable.null
      };
    }
    and nullableOfNullable = {
      let value = value##nullableOfNullable;
      switch (value) {
      | Some(value) =>
        Js.Nullable.return(
          value
          |> Js.Array.map(value =>
               switch (value) {
               | Some(value) => Js.Nullable.return(value)
               | None => Js.Nullable.null
               }
             ),
        )
      | None => Js.Nullable.null
      };
    };
    {
      "nullableOfNullable": nullableOfNullable,
      "nullableOfNonNullable": nullableOfNonNullable,
    };
  };
  let verifyName =
    fun
    | `ListFragment => ();
  external unsafe_fromJson: Js.Json.t => Raw.t = "%identity";
  external toJson: Raw.t => Js.Json.t = "%identity";
  module Z__INTERNAL = {
    type root = t;
    type nonrec graphql;
    /**```
Lists {
  nullableOfNullable: [String]
  nullableOfNonNullable: [String!]
  nonNullableOfNullable: [String]!
  nonNullableOfNonNullable: [String!]!
  listWithArg: [String]
}
```*/
    let graphql: graphql = Obj.magic(0);
    type nonrec graphql_module;
    /****--- graphql-ppx module ---**

The contents of this module are automatically generated by `graphql-ppx`.
The following is simply an overview of the most important variables and types that you can access from this module.

```
module GraphQL {
  /**
  The GraphQL query string
  */
  let query: string;

  /**
  This is the main type of the result you will get back.
  You can hover above the identifier key (e.g. query or mutation) to see the fully generated type for your module.
  */
  type t;

  /**
  Parse the JSON GraphQL data to ReasonML data types
  */
  let parse: Raw.t => t;

  /**
  Serialize the ReasonML GraphQL data that was parsed using the parse function back to the original JSON compatible data
  */
  let serialize: t => Raw.t;

  /**
  This is the JSON compatible type of the GraphQL data.
  It should not be necessary to access the types inside for normal use cases.
  */
  module Raw: { type t; };
}
```*/
    let graphql_module: graphql_module = Obj.magic(0);
  };
};
module MyQuery5 = {
  [@ocaml.warning "-32"];
  module Raw = {
    type t = {. "lists": ListFragment.Raw.t};
    type t_variables = unit;
  };
  type t = {. "lists": ListFragment.t};
  /**The GraphQL query string*/
  let query: string = [%raw
    "require(\"gatsby\")`\n  query MyQuery5  {\n    lists  {\n      ...ListFragment\n    }\n  }\n`"
  ];
  type t_variables = unit;
  /**Parse the JSON GraphQL data to ReasonML data types*/
  let parse = (value: Raw.t): t => {
    let lists = {
      let value = value##lists;

      ListFragment.verifyArgsAndParse(~fragmentName=`ListFragment, value);
    };
    {"lists": lists};
  };
  /**Serialize the ReasonML GraphQL data that was parsed using the parse function back to the original JSON compatible data */
  let serialize = (value: t): Raw.t => {
    let lists = {
      let value = value##lists;
      ListFragment.serialize(value);
    };
    {"lists": lists};
  };
  let serializeVariables = () => ();
  let makeVariables = () => ();
  let makeDefaultVariables = () => makeVariables();
  external unsafe_fromJson: Js.Json.t => Raw.t = "%identity";
  external toJson: Raw.t => Js.Json.t = "%identity";
  external variablesToJson: Raw.t_variables => Js.Json.t = "%identity";
  module Z__INTERNAL = {
    type root = t;
    type nonrec graphql_module;
    /****--- graphql-ppx module ---**

The contents of this module are automatically generated by `graphql-ppx`.
The following is simply an overview of the most important variables and types that you can access from this module.

```
module GraphQL {
  /**
  The GraphQL query string
  */
  let query: string;

  /**
  This is the main type of the result you will get back.
  You can hover above the identifier key (e.g. query or mutation) to see the fully generated type for your module.
  */
  type t;

  /**
  Parse the JSON GraphQL data to ReasonML data types
  */
  let parse: Raw.t => t;

  /**
  Serialize the ReasonML GraphQL data that was parsed using the parse function back to the original JSON compatible data
  */
  let serialize: t => Raw.t;

  /**
  This is the JSON compatible type of the GraphQL data.
  It should not be necessary to access the types inside for normal use cases.
  */
  module Raw: { type t; };
}
```*/
    let graphql_module: graphql_module = Obj.magic(0);
  };
};
module MyQuery6 = {
  [@ocaml.warning "-32"];
  module Raw = {
    type t = {. "lists": ListFragment.Raw.t};
    type t_variables = unit;
  };
  type t = {. "lists": ListFragment.t};
  /**The GraphQL query string*/
  let query: string =
    [%raw
      "(frag_0) => require(\"gatsby\")`\n  query MyQuery6  {\n    lists  {\n      ...ListFragment\n    }\n  }\n  ${frag_0}\n`"
    ](
      ListFragment.query,
    );
  type t_variables = unit;
  /**Parse the JSON GraphQL data to ReasonML data types*/
  let parse = (value: Raw.t): t => {
    let lists = {
      let value = value##lists;

      ListFragment.verifyArgsAndParse(~fragmentName=`ListFragment, value);
    };
    {"lists": lists};
  };
  /**Serialize the ReasonML GraphQL data that was parsed using the parse function back to the original JSON compatible data */
  let serialize = (value: t): Raw.t => {
    let lists = {
      let value = value##lists;
      ListFragment.serialize(value);
    };
    {"lists": lists};
  };
  let serializeVariables = () => ();
  let makeVariables = () => ();
  let makeDefaultVariables = () => makeVariables();
  external unsafe_fromJson: Js.Json.t => Raw.t = "%identity";
  external toJson: Raw.t => Js.Json.t = "%identity";
  external variablesToJson: Raw.t_variables => Js.Json.t = "%identity";
  module Z__INTERNAL = {
    type root = t;
    type nonrec graphql_module;
    /****--- graphql-ppx module ---**

The contents of this module are automatically generated by `graphql-ppx`.
The following is simply an overview of the most important variables and types that you can access from this module.

```
module GraphQL {
  /**
  The GraphQL query string
  */
  let query: string;

  /**
  This is the main type of the result you will get back.
  You can hover above the identifier key (e.g. query or mutation) to see the fully generated type for your module.
  */
  type t;

  /**
  Parse the JSON GraphQL data to ReasonML data types
  */
  let parse: Raw.t => t;

  /**
  Serialize the ReasonML GraphQL data that was parsed using the parse function back to the original JSON compatible data
  */
  let serialize: t => Raw.t;

  /**
  This is the JSON compatible type of the GraphQL data.
  It should not be necessary to access the types inside for normal use cases.
  */
  module Raw: { type t; };
}
```*/
    let graphql_module: graphql_module = Obj.magic(0);
  };
};
module MyQuery7 = {
  [@ocaml.warning "-32"];
  module Raw = {
    type t = {. "lists": ListFragment.Raw.t};
    type t_variables = unit;
  };
  type t = {. "lists": ListFragment.t};
  /**The GraphQL query string*/
  let query: string = [%raw
    "graphql`\n  query MyQuery7  {\n    lists  {\n      ...ListFragment\n    }\n  }\n`"
  ];
  type t_variables = unit;
  /**Parse the JSON GraphQL data to ReasonML data types*/
  let parse = (value: Raw.t): t => {
    let lists = {
      let value = value##lists;

      ListFragment.verifyArgsAndParse(~fragmentName=`ListFragment, value);
    };
    {"lists": lists};
  };
  /**Serialize the ReasonML GraphQL data that was parsed using the parse function back to the original JSON compatible data */
  let serialize = (value: t): Raw.t => {
    let lists = {
      let value = value##lists;
      ListFragment.serialize(value);
    };
    {"lists": lists};
  };
  let serializeVariables = () => ();
  let makeVariables = () => ();
  let makeDefaultVariables = () => makeVariables();
  external unsafe_fromJson: Js.Json.t => Raw.t = "%identity";
  external toJson: Raw.t => Js.Json.t = "%identity";
  external variablesToJson: Raw.t_variables => Js.Json.t = "%identity";
  module Z__INTERNAL = {
    type root = t;
    type nonrec graphql_module;
    /****--- graphql-ppx module ---**

The contents of this module are automatically generated by `graphql-ppx`.
The following is simply an overview of the most important variables and types that you can access from this module.

```
module GraphQL {
  /**
  The GraphQL query string
  */
  let query: string;

  /**
  This is the main type of the result you will get back.
  You can hover above the identifier key (e.g. query or mutation) to see the fully generated type for your module.
  */
  type t;

  /**
  Parse the JSON GraphQL data to ReasonML data types
  */
  let parse: Raw.t => t;

  /**
  Serialize the ReasonML GraphQL data that was parsed using the parse function back to the original JSON compatible data
  */
  let serialize: t => Raw.t;

  /**
  This is the JSON compatible type of the GraphQL data.
  It should not be necessary to access the types inside for normal use cases.
  */
  module Raw: { type t; };
}
```*/
    let graphql_module: graphql_module = Obj.magic(0);
  };
};

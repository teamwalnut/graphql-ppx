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
  [@ocaml.warning "-32"];
  module Raw = {
    type t = {scalarsInput: string};
    type t_variables = {arg: t_variables_VariousScalarsInput}
    and t_variables_VariousScalarsInput = {
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
  };
  /**The GraphQL query string*/
  let query = [%raw
    "require(\"gql\")`\n  query ($arg: VariousScalarsInput!)  {\n    scalarsInput(arg: $arg)\n  }\n`"
  ];
  type t = {scalarsInput: string};
  type t_variables = {arg: t_variables_VariousScalarsInput}
  and t_variables_VariousScalarsInput = {
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
  /**Parse the JSON GraphQL data to ReasonML data types*/
  let parse = (value: Raw.t): t => (
    {
      scalarsInput: {
        let value = (value: Raw.t).scalarsInput;
        value;
      },
    }: t
  );
  /**Serialize the ReasonML GraphQL data that was parsed using the parse function back to the original JSON compatible data */
  let serialize = (value: t): Raw.t => (
    {
      let scalarsInput = {
        let value = (value: t).scalarsInput;
        value;
      };
      {scalarsInput: scalarsInput};
    }: Raw.t
  );
  let rec serializeVariables: t_variables => Raw.t_variables =
    inp => {
      arg:
        (a => serializeInputObjectVariousScalarsInput(a))(
          (inp: t_variables).arg,
        ),
    }
  and serializeInputObjectVariousScalarsInput:
    t_variables_VariousScalarsInput => Raw.t_variables_VariousScalarsInput =
    inp => {
      nullableString:
        (
          a =>
            switch (a) {
            | None => Js.Nullable.undefined
            | Some(b) => Js.Nullable.return((a => a)(b))
            }
        )(
          (inp: t_variables_VariousScalarsInput).nullableString,
        ),
      string: (a => a)((inp: t_variables_VariousScalarsInput).string),
      nullableInt:
        (
          a =>
            switch (a) {
            | None => Js.Nullable.undefined
            | Some(b) => Js.Nullable.return((a => a)(b))
            }
        )(
          (inp: t_variables_VariousScalarsInput).nullableInt,
        ),
      int: (a => a)((inp: t_variables_VariousScalarsInput).int),
      nullableFloat:
        (
          a =>
            switch (a) {
            | None => Js.Nullable.undefined
            | Some(b) => Js.Nullable.return((a => a)(b))
            }
        )(
          (inp: t_variables_VariousScalarsInput).nullableFloat,
        ),
      float: (a => a)((inp: t_variables_VariousScalarsInput).float),
      nullableBoolean:
        (
          a =>
            switch (a) {
            | None => Js.Nullable.undefined
            | Some(b) => Js.Nullable.return((a => a)(b))
            }
        )(
          (inp: t_variables_VariousScalarsInput).nullableBoolean,
        ),
      boolean: (a => a)((inp: t_variables_VariousScalarsInput).boolean),
      nullableID:
        (
          a =>
            switch (a) {
            | None => Js.Nullable.undefined
            | Some(b) => Js.Nullable.return((a => a)(b))
            }
        )(
          (inp: t_variables_VariousScalarsInput).nullableID,
        ),
      id: (a => a)((inp: t_variables_VariousScalarsInput).id),
    };
  let makeVariables = (~arg, ()) =>
    serializeVariables({arg: arg}: t_variables)
  and makeInputObjectVariousScalarsInput =
      (
        ~nullableString=?,
        ~string,
        ~nullableInt=?,
        ~int,
        ~nullableFloat=?,
        ~float,
        ~nullableBoolean=?,
        ~boolean,
        ~nullableID=?,
        ~id,
        (),
      )
      : t_variables_VariousScalarsInput => {
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
  external unsafe_fromJson: Js.Json.t => Raw.t = "%identity";
  external toJson: Raw.t => Js.Json.t = "%identity";
  external variablesToJson: Raw.t_variables => Js.Json.t = "%identity";
  module Z__INTERNAL = {
    type nonrec _graphql_arg_93;
    /**Variable **$arg** has the following graphql type:

```
VariousScalarsInput {
  nullableString: String
  string: String!
  nullableInt: Int
  int: Int!
  nullableFloat: Float
  float: Float!
  nullableBoolean: Boolean
  boolean: Boolean!
  nullableID: ID
  id: ID!
}!
```*/
    let _graphql_arg_93: _graphql_arg_93 = Obj.magic(0);
    type nonrec _graphql_arg_88;
    /**Argument **arg** on field **scalarsInput** has the following graphql type:

```
VariousScalarsInput {
  nullableString: String
  string: String!
  nullableInt: Int
  int: Int!
  nullableFloat: Float
  float: Float!
  nullableBoolean: Boolean
  boolean: Boolean!
  nullableID: ID
  id: ID!
}!
```*/
    let _graphql_arg_88: _graphql_arg_88 = Obj.magic(0);
    type root = t;
    type nonrec _graphql_VariousScalarsInput_47;
    /**```
VariousScalarsInput {
  nullableString: String
  string: String!
  nullableInt: Int
  int: Int!
  nullableFloat: Float
  float: Float!
  nullableBoolean: Boolean
  boolean: Boolean!
  nullableID: ID
  id: ID!
}
```*/
    let _graphql_VariousScalarsInput_47: _graphql_VariousScalarsInput_47 =
      Obj.magic(0);
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

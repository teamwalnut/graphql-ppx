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
    type t = {listsInput: string};
    type t_variables = {arg: t_variables_ListsInput}
    and t_variables_ListsInput = {
      nullableOfNullable: Js.Nullable.t(array(Js.Nullable.t(string))),
      nullableOfNonNullable: Js.Nullable.t(array(string)),
      nonNullableOfNullable: array(Js.Nullable.t(string)),
      nonNullableOfNonNullable: array(string),
    };
  };
  let query = "query ($arg: ListsInput!)  {\nlistsInput(arg: $arg)  \n}\n";
  type t = {listsInput: string};
  type t_variables = {arg: t_variables_ListsInput}
  and t_variables_ListsInput = {
    nullableOfNullable: option(array(option(string))),
    nullableOfNonNullable: option(array(string)),
    nonNullableOfNullable: array(option(string)),
    nonNullableOfNonNullable: array(string),
  };
  let parse: Raw.t => t =
    (value) => (
      {
        listsInput: {
          let value = (value: Raw.t).listsInput;
          value;
        },
      }: t
    );
  let serialize: t => Raw.t =
    (value) => (
      {
        let listsInput = {
          let value = (value: t).listsInput;
          value;
        };
        {listsInput: listsInput};
      }: Raw.t
    );
  let rec serializeVariables: t_variables => Raw.t_variables =
    inp => {
      arg: (a => serializeInputObjectListsInput(a))((inp: t_variables).arg),
    }
  and serializeInputObjectListsInput:
    t_variables_ListsInput => Raw.t_variables_ListsInput =
    inp => {
      nullableOfNullable:
        (
          a =>
            switch (a) {
            | None => Js.Nullable.undefined
            | Some(b) =>
              Js.Nullable.return(
                (
                  a =>
                    Array.map(
                      b =>
                        (
                          a =>
                            switch (a) {
                            | None => Js.Nullable.undefined
                            | Some(b) => Js.Nullable.return((a => a)(b))
                            }
                        )(
                          b,
                        ),
                      a,
                    )
                )(
                  b,
                ),
              )
            }
        )(
          (inp: t_variables_ListsInput).nullableOfNullable,
        ),
      nullableOfNonNullable:
        (
          a =>
            switch (a) {
            | None => Js.Nullable.undefined
            | Some(b) =>
              Js.Nullable.return((a => Array.map(b => (a => a)(b), a))(b))
            }
        )(
          (inp: t_variables_ListsInput).nullableOfNonNullable,
        ),
      nonNullableOfNullable:
        (
          a =>
            Array.map(
              b =>
                (
                  a =>
                    switch (a) {
                    | None => Js.Nullable.undefined
                    | Some(b) => Js.Nullable.return((a => a)(b))
                    }
                )(
                  b,
                ),
              a,
            )
        )(
          (inp: t_variables_ListsInput).nonNullableOfNullable,
        ),
      nonNullableOfNonNullable:
        (a => Array.map(b => (a => a)(b), a))(
          (inp: t_variables_ListsInput).nonNullableOfNonNullable,
        ),
    };
  let makeVariables = (~arg, ()) =>
    serializeVariables({arg: arg}: t_variables)
  and makeInputObjectListsInput =
      (
        ~nullableOfNullable=?,
        ~nullableOfNonNullable=?,
        ~nonNullableOfNullable,
        ~nonNullableOfNonNullable,
        (),
      )
      : t_variables_ListsInput => {
    nullableOfNullable,
    nullableOfNonNullable,
    nonNullableOfNullable,
    nonNullableOfNonNullable,
  };
  let definition = (parse, query, serialize);
  module Z__INTERNAL = {
    type nonrec _graphql_arg_82;
    /**Variable **$arg** has the following graphql type:

```
ListsInput {
  nullableOfNullable: [String]
  nullableOfNonNullable: [String!]
  nonNullableOfNullable: [String]!
  nonNullableOfNonNullable: [String!]!
}!
```*/
    let _graphql_arg_82: _graphql_arg_82 = Obj.magic(0);
    type nonrec _graphql_arg_77;
    /**Argument **arg** on field **listsInput** has the following graphql type:

```
ListsInput {
  nullableOfNullable: [String]
  nullableOfNonNullable: [String!]
  nonNullableOfNullable: [String]!
  nonNullableOfNonNullable: [String!]!
}!
```*/
    let _graphql_arg_77: _graphql_arg_77 = Obj.magic(0);
    type root = t;
    type nonrec _graphql_ListsInput_47;
    /**```
ListsInput {
  nullableOfNullable: [String]
  nullableOfNonNullable: [String!]
  nonNullableOfNullable: [String]!
  nonNullableOfNonNullable: [String!]!
}
```*/
    let _graphql_ListsInput_47: _graphql_ListsInput_47 = Obj.magic(0);
  };
};

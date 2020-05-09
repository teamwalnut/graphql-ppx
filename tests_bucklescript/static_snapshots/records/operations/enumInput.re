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
    type t = {enumInput: string};
    type t_variables = {arg: string};
  };
  let query = "query ($arg: SampleField!)  {\nenumInput(arg: $arg)  \n}\n";
  type t = {enumInput: string};
  type t_variables = {arg: [ | `FIRST | `SECOND | `THIRD]};
  let parse: Raw.t => t =
    (value) => (
      {
        enumInput: {
          let value = (value: Raw.t).enumInput;
          value;
        },
      }: t
    );
  let serialize: t => Raw.t =
    (value) => (
      {
        let enumInput = {
          let value = (value: t).enumInput;
          value;
        };
        {enumInput: enumInput};
      }: Raw.t
    );
  let serializeVariables: t_variables => Raw.t_variables =
    inp => {
      arg:
        (
          a =>
            switch (a) {
            | `FIRST => "FIRST"
            | `SECOND => "SECOND"
            | `THIRD => "THIRD"
            }
        )(
          (inp: t_variables).arg,
        ),
    };
  let makeVariables = (~arg, ()) =>
    serializeVariables({arg: arg}: t_variables);
  let definition = (parse, query, serialize);
  module Z__INTERNAL = {
    type nonrec _graphql_arg_82;
    /**Variable **$arg** has the following graphql type:

```
SampleField {
  FIRST
  SECOND
  THIRD
}!
```*/
    let _graphql_arg_82: _graphql_arg_82 = Obj.magic(0);
    type nonrec _graphql_arg_77;
    /**Argument **arg** on field **enumInput** has the following graphql type:

```
SampleField {
  FIRST
  SECOND
  THIRD
}!
```*/
    let _graphql_arg_77: _graphql_arg_77 = Obj.magic(0);
    type root = t;
    type nonrec _graphql_SampleField_47;
    /**```
SampleField {
  FIRST
  SECOND
  THIRD
}
```*/
    let _graphql_SampleField_47: _graphql_SampleField_47 = Obj.magic(0);
  };
};

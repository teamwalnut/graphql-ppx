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
    type t = {defaultObjectValueOnScalar: string};
  };
  let query = "query   {\ndefaultObjectValueOnScalar(filter: {some: {json: \"value\"}}, arg: {field: \"otherValue\"})  \n}\n";
  type t = {defaultObjectValueOnScalar: string};
  let parse: Raw.t => t =
    (value) => (
      {
        defaultObjectValueOnScalar: {
          let value = (value: Raw.t).defaultObjectValueOnScalar;
          value;
        },
      }: t
    );
  let serialize: t => Raw.t =
    (value) => (
      {
        let defaultObjectValueOnScalar = {
          let value = (value: t).defaultObjectValueOnScalar;
          value;
        };
        {defaultObjectValueOnScalar: defaultObjectValueOnScalar};
      }: Raw.t
    );
  let definition = (parse, query, serialize);
  module Z__INTERNAL = {
    type nonrec _graphql_filter_92;
    /**Argument **filter** on field **defaultObjectValueOnScalar** has the following graphql type:

```
JsonScalar!
```*/
    let _graphql_filter_92: _graphql_filter_92 = Obj.magic(0);
    type nonrec _graphql_arg_140;
    /**Argument **arg** on field **defaultObjectValueOnScalar** has the following graphql type:

```
EmbeddedInput {
  field: String
}!
```*/
    let _graphql_arg_140: _graphql_arg_140 = Obj.magic(0);
    type root = t;
  };
};

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
    type t_pokemon = {
      __typename: string,
      id: string,
      name: Js.Nullable.t(string),
    };
    type t = {pokemon: Js.Nullable.t(t_pokemon)};
  };
  let query = "query   {\npokemon(name: \"Pikachu\")  {\n__typename  \nid  \nname  \n}\n\n}\n";
  type t_pokemon = {
    __typename: string,
    id: string,
    name: option(string),
  };
  type t = {pokemon: option(t_pokemon)};
  let parse: Raw.t => t =
    (value) => (
      {
        pokemon: {
          let value = (value: Raw.t).pokemon;
          switch (Js.toOption(value)) {
          | Some(value) =>
            Some(
              {
                __typename: {
                  let value = (value: Raw.t_pokemon).__typename;
                  value;
                },
                id: {
                  let value = (value: Raw.t_pokemon).id;
                  value;
                },
                name: {
                  let value = (value: Raw.t_pokemon).name;
                  switch (Js.toOption(value)) {
                  | Some(value) => Some(value)
                  | None => None
                  };
                },
              }: t_pokemon,
            )
          | None => None
          };
        },
      }: t
    );
  let serialize: t => Raw.t =
    (value) => (
      {
        let pokemon = {
          let value = (value: t).pokemon;
          switch (value) {
          | Some(value) =>
            Js.Nullable.return(
              {
                let name = {
                  let value = (value: t_pokemon).name;
                  switch (value) {
                  | Some(value) => Js.Nullable.return(value)
                  | None => Js.Nullable.null
                  };
                }
                and id = {
                  let value = (value: t_pokemon).id;
                  value;
                }
                and __typename = {
                  let value = (value: t_pokemon).__typename;
                  value;
                };
                {__typename, id, name};
              }: Raw.t_pokemon,
            )
          | None => Js.Nullable.null
          };
        };
        {pokemon: pokemon};
      }: Raw.t
    );
  let definition = (parse, query, serialize);
  module Z__INTERNAL = {
    type nonrec _graphql_name_52;
    /**Argument **name** on field **pokemon** has the following graphql type:

```
String
```*/
    let _graphql_name_52: _graphql_name_52 = Obj.magic(0);
    type root = t;
  };
};

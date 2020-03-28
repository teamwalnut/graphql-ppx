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
  let query = "query   {\npokemon(name: \"Pikachu\")  {\nid  \nname  \n}\n\n}\n";
  type raw_t;
  type t = {pokemon: option(t_pokemon)}
  and t_pokemon = {
    id: string,
    name: option(string),
  };
  let parse: Js.Json.t => t =
    (value) => (
      {

        pokemon: {
          let value = Js.Dict.unsafeGet(Obj.magic(value), "pokemon");

          switch (Js.toOption(Obj.magic(value): Js.Nullable.t('a))) {
          | Some(_) =>
            Some(
              {

                id: {
                  let value = Js.Dict.unsafeGet(Obj.magic(value), "id");

                  (Obj.magic(value): string);
                },

                name: {
                  let value = Js.Dict.unsafeGet(Obj.magic(value), "name");

                  switch (Js.toOption(Obj.magic(value): Js.Nullable.t('a))) {
                  | Some(_) => Some(Obj.magic(value): string)
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
  let makeVar = (~f, ()) => f(Js.Json.null);
  let definition = (parse, query, makeVar);
};

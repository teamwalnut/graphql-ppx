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
    type t = {pokemon: Js.Nullable.t(t_pokemon)}
    and t_pokemon = {
      id: string,
      name: Js.Nullable.t(string),
    };
  };
  let query = "query   {\npokemon(name: \"Pikachu\")  {\nid  \nname  \n}\n\n}\n";
  type t = {pokemon: option(t_pokemon)}
  and t_pokemon = {
    id: string,
    name: option(string),
  };
  let parse: Raw.t => t =
    (value) => (
      {

        pokemon: {
          let value = (value: Raw.t).pokemon;

          switch (Js.toOption(value)) {
          | Some(value) =>
            Some(
              {

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

        pokemon: {
          let value = (value: t).pokemon;

          switch (value) {
          | Some(value) =>
            Js.Nullable.return(
              {

                id: {
                  let value = (value: t_pokemon).id;

                  value;
                },

                name: {
                  let value = (value: t_pokemon).name;

                  switch (value) {
                  | Some(value) => Js.Nullable.return(value)
                  | None => Js.Nullable.null
                  };
                },
              }: Raw.t_pokemon,
            )
          | None => Js.Nullable.null
          };
        },
      }: Raw.t
    );
  let makeVar = (~f, ()) => f(Js.Json.null);
  let definition = (parse, query, makeVar);
};

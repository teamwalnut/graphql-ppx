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
      .
      "id": string,
      "name": Js.Nullable.t(string),
    };
    type t = {. "pokemon": Js.Nullable.t(t_pokemon)};
  };
  let query = "query   {\npokemon(name: \"Pikachu\")  {\nid  \nname  \n}\n\n}\n";
  type t_pokemon = {
    .
    "id": string,
    "name": option(string),
  };
  type t = {. "pokemon": option(t_pokemon)};
  let parse: Raw.t => t =
    value => {
      let pokemon = {
        let value = value##pokemon;
        switch (Js.toOption(value)) {
        | Some(value) =>
          Some(
            {
              let name = {
                let value = value##name;
                switch (Js.toOption(value)) {
                | Some(value) => Some(value)
                | None => None
                };
              }
              and id = {
                let value = value##id;
                value;
              };
              {"id": id, "name": name};
            },
          )
        | None => None
        };
      };
      {"pokemon": pokemon};
    };
  let serialize: t => Raw.t =
    value => {
      let pokemon = {
        let value = value##pokemon;
        switch (value) {
        | Some(value) =>
          Js.Nullable.return(
            {
              let name = {
                let value = value##name;
                switch (value) {
                | Some(value) => Js.Nullable.return(value)
                | None => Js.Nullable.null
                };
              }
              and id = {
                let value = value##id;
                value;
              };
              {"id": id, "name": name};
            },
          )
        | None => Js.Nullable.null
        };
      };
      {"pokemon": pokemon};
    };
  let definition = (parse, query, serialize);
  module Z__INTERNAL = {
    type root = t;
  };
};

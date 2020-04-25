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
    type t_variables = {
      .
      "id": Js.Nullable.t(string),
      "name": Js.Nullable.t(string),
    };
  };
  let query = "query pokemon($id: String, $name: String)  {\npokemon(name: $name, id: $id)  {\nid  \nname  \n}\n\n}\n";
  type t_pokemon = {
    .
    "id": string,
    "name": option(string),
  };
  type t = {. "pokemon": option(t_pokemon)};
  type t_variables = {
    .
    "id": option(string),
    "name": option(string),
  };
  let parse: Raw.t => t =
    value => {

      "pokemon": {
        let value = value##pokemon;

        switch (Js.toOption(value)) {
        | Some(value) =>
          Some({

            "id": {
              let value = value##id;

              value;
            },

            "name": {
              let value = value##name;

              switch (Js.toOption(value)) {
              | Some(value) => Some(value)
              | None => None
              };
            },
          })
        | None => None
        };
      },
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
              {

                "id": id,

                "name": name,
              };
            },
          )
        | None => Js.Nullable.null
        };
      };
      {

        "pokemon": pokemon,
      };
    };
  let serializeVariables: t_variables => Raw.t_variables =
    inp => {

      id:
        (
          a =>
            switch (a) {
            | None => Js.Nullable.undefined
            | Some(b) => Js.Nullable.return((a => a)(b))
            }
        )(
          inp##id,
        ),

      name:
        (
          a =>
            switch (a) {
            | None => Js.Nullable.undefined
            | Some(b) => Js.Nullable.return((a => a)(b))
            }
        )(
          inp##name,
        ),
    };
  let makeVariables = (~id=?, ~name=?, ()) =>
    serializeVariables(
      {

        "id": id,

        "name": name,
      }: t_variables,
    );
  let definition = (parse, query, serialize);
};

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
    type t_variables = {
      id: Js.Nullable.t(string),
      name: Js.Nullable.t(string),
    };
  };
  let query = "query pokemon($id: String, $name: String)  {\npokemon(name: $name, id: $id)  {\n__typename  \nid  \nname  \n}\n\n}\n";
  type t_pokemon = {
    __typename: string,
    id: string,
    name: option(string),
  };
  type t = {pokemon: option(t_pokemon)};
  type t_variables = {
    id: option(string),
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
                {

                  __typename,

                  id,

                  name,
                };
              }: Raw.t_pokemon,
            )
          | None => Js.Nullable.null
          };
        };
        {

          pokemon: pokemon,
        };
      }: Raw.t
    );
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
          (inp: t_variables).id,
        ),

      name:
        (
          a =>
            switch (a) {
            | None => Js.Nullable.undefined
            | Some(b) => Js.Nullable.return((a => a)(b))
            }
        )(
          (inp: t_variables).name,
        ),
    };
  let makeVariables = (~id=?, ~name=?, ()) =>
    serializeVariables(
      {

        id,

        name,
      }: t_variables,
    );
  let definition = (parse, query, serialize);
};

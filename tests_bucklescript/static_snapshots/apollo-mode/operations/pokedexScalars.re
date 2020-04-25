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
    type t = {
      __typename: string,
      pokemon: Js.Nullable.t(t_pokemon),
    }
    and t_pokemon = {
      __typename: string,
      id: string,
      name: Js.Nullable.t(string),
    };
  };
  let query = "query pokemon($id: String, $name: String)  {\n__typename  \npokemon(name: $name, id: $id)  {\n__typename  \nid  \nname  \n}\n\n}\n";
  type t = {
    __typename: string,
    pokemon: option(t_pokemon),
  }
  and t_pokemon = {
    __typename: string,
    id: string,
    name: option(string),
  };
  type t_variables = {
    id: option(string),
    name: option(string),
  };
  let parse: Raw.t => t =
    (value) => (
      {

        __typename: {
          let value = (value: Raw.t).__typename;

          value;
        },

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
        }
        and __typename = {
          let value = (value: t).__typename;

          value;
        };
        {

          __typename,

          pokemon,
        };
      }: Raw.t
    );
  let serializeVariables: t_variables => Js.Json.t =
    inp =>
      [|
        (
          "id",
          (
            a =>
              switch (a) {
              | None => None
              | Some(b) => (a => Some(Js.Json.string(a)))(b)
              }
          )(
            inp.id,
          ),
        ),
        (
          "name",
          (
            a =>
              switch (a) {
              | None => None
              | Some(b) => (a => Some(Js.Json.string(a)))(b)
              }
          )(
            inp.name,
          ),
        ),
      |]
      |> Js.Array.filter(
           fun
           | (_, None) => false
           | (_, Some(_)) => true,
         )
      |> Js.Array.map(
           fun
           | (k, Some(v)) => (k, v)
           | (k, None) => (k, Js.Json.null),
         )
      |> Js.Dict.fromArray
      |> Js.Json.object_;
  let makeVar = (~f, ~id=?, ~name=?, ()) =>
    f(
      serializeVariables(
        {

          id,

          name,
        }: t_variables,
      ),
    );
  let definition = (parse, query, makeVar);
  let makeVariables = makeVar(~f=f => f);
};

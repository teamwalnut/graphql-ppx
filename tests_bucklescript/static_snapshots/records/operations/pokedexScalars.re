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
  let query = "query pokemon($id: String, $name: String)  {\npokemon(name: $name, id: $id)  {\nid  \nname  \n}\n\n}\n";
  type raw_t;
  type t = {pokemon: option(t_pokemon)}
  and t_pokemon = {
    id: string,
    name: option(string),
  };
  type t_variables = {
    id: option(string),
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

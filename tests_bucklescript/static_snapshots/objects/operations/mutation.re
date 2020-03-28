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
  let query = "mutation   {\nmutationWithError  {\nvalue  {\nstringField  \n}\n\nerrors  {\nfield  \nmessage  \n}\n\n}\n\n}\n";
  type raw_t;
  type t = {. "mutationWithError": t_mutationWithError}
  and t_mutationWithError = {
    .
    "value": option(t_mutationWithError_value),
    "errors": option(array(t_mutationWithError_errors)),
  }
  and t_mutationWithError_errors = {
    .
    "field": [ | `FutureAddedValue(string) | `FIRST | `SECOND | `THIRD],
    "message": string,
  }
  and t_mutationWithError_value = {. "stringField": string};
  let parse: Js.Json.t => t =
    value => {
      [@metaloc loc]
      let value = value |> Js.Json.decodeObject |> Js.Option.getExn;
      {

        "mutationWithError": {
          let value =
            Js.Dict.unsafeGet(Obj.magic(value), "mutationWithError");
          [@metaloc loc]
          let value = value |> Js.Json.decodeObject |> Js.Option.getExn;
          {

            "value": {
              let value = Js.Dict.unsafeGet(Obj.magic(value), "value");

              switch (Js.toOption(Obj.magic(value): Js.Nullable.t('a))) {
              | Some(_) =>
                Some(
                  {
                    [@metaloc loc]
                    let value =
                      value |> Js.Json.decodeObject |> Js.Option.getExn;
                    {

                      "stringField": {
                        let value =
                          Js.Dict.unsafeGet(Obj.magic(value), "stringField");

                        (Obj.magic(value): string);
                      },
                    };
                  },
                )
              | None => None
              };
            },

            "errors": {
              let value = Js.Dict.unsafeGet(Obj.magic(value), "errors");

              switch (Js.toOption(Obj.magic(value): Js.Nullable.t('a))) {
              | Some(_) =>
                Some(
                  Obj.magic(value)
                  |> Js.Array.map(value =>
                       [@metaloc loc]
                       let value =
                         value |> Js.Json.decodeObject |> Js.Option.getExn;
                       {

                         "field": {
                           let value =
                             Js.Dict.unsafeGet(Obj.magic(value), "field");
                           (
                             switch (Obj.magic(value: string)) {
                             | "FIRST" => `FIRST
                             | "SECOND" => `SECOND
                             | "THIRD" => `THIRD
                             | other => `FutureAddedValue(other)
                             }: [
                               | `FutureAddedValue(string)
                               | `FIRST
                               | `SECOND
                               | `THIRD
                             ]
                           );
                         },

                         "message": {
                           let value =
                             Js.Dict.unsafeGet(Obj.magic(value), "message");

                           (Obj.magic(value): string);
                         },
                       };
                     ),
                )
              | None => None
              };
            },
          };
        },
      };
    };
  let makeVar = (~f, ()) => f(Js.Json.null);
  let definition = (parse, query, makeVar);
};

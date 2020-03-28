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
  let query = "query   {\nlists  {\nnullableOfNullable  \nnullableOfNonNullable  \nnonNullableOfNullable  \nnonNullableOfNonNullable  \n}\n\n}\n";
  type raw_t;
  type t = {lists: t_lists}
  and t_lists = {
    nullableOfNullable: option(array(option(string))),
    nullableOfNonNullable: option(array(string)),
    nonNullableOfNullable: array(option(string)),
    nonNullableOfNonNullable: array(string),
  };
  let parse: Js.Json.t => t =
    (value) => (
      {

        lists: {
          let value = Js.Dict.unsafeGet(Obj.magic(value), "lists");
          (
            {

              nullableOfNullable: {
                let value =
                  Js.Dict.unsafeGet(Obj.magic(value), "nullableOfNullable");

                switch (Js.toOption(Obj.magic(value): Js.Nullable.t('a))) {
                | Some(_) =>
                  Some(
                    Obj.magic(value)
                    |> Js.Array.map(value =>
                         switch (
                           Js.toOption(Obj.magic(value): Js.Nullable.t('a))
                         ) {
                         | Some(_) => Some(Obj.magic(value): string)
                         | None => None
                         }
                       ),
                  )
                | None => None
                };
              },

              nullableOfNonNullable: {
                let value =
                  Js.Dict.unsafeGet(
                    Obj.magic(value),
                    "nullableOfNonNullable",
                  );

                switch (Js.toOption(Obj.magic(value): Js.Nullable.t('a))) {
                | Some(_) =>
                  Some(
                    Obj.magic(value)
                    |> Js.Array.map((value) => (Obj.magic(value): string)),
                  )
                | None => None
                };
              },

              nonNullableOfNullable: {
                let value =
                  Js.Dict.unsafeGet(
                    Obj.magic(value),
                    "nonNullableOfNullable",
                  );

                Obj.magic(value)
                |> Js.Array.map(value =>
                     switch (
                       Js.toOption(Obj.magic(value): Js.Nullable.t('a))
                     ) {
                     | Some(_) => Some(Obj.magic(value): string)
                     | None => None
                     }
                   );
              },

              nonNullableOfNonNullable: {
                let value =
                  Js.Dict.unsafeGet(
                    Obj.magic(value),
                    "nonNullableOfNonNullable",
                  );

                Obj.magic(value)
                |> Js.Array.map((value) => (Obj.magic(value): string));
              },
            }: t_lists
          );
        },
      }: t
    );
  let makeVar = (~f, ()) => f(Js.Json.null);
  let definition = (parse, query, makeVar);
};

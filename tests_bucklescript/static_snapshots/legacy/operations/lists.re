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
    type t_lists = {
      .
      "nullableOfNullable": Js.Nullable.t(array(Js.Nullable.t(string))),
      "nullableOfNonNullable": Js.Nullable.t(array(string)),
      "nonNullableOfNullable": array(Js.Nullable.t(string)),
      "nonNullableOfNonNullable": array(string),
    };
    type t = {. "lists": t_lists};
  };
  let query = "query   {\nlists  {\nnullableOfNullable  \nnullableOfNonNullable  \nnonNullableOfNullable  \nnonNullableOfNonNullable  \n}\n\n}\n";
  type t_lists = {
    .
    "nullableOfNullable": option(array(option(string))),
    "nullableOfNonNullable": option(array(string)),
    "nonNullableOfNullable": array(option(string)),
    "nonNullableOfNonNullable": array(string),
  };
  type t = {. "lists": t_lists};
  let parse: Raw.t => t =
    value => {
      "lists": {
        let value = value##lists;
        {
          "nullableOfNullable": {
            let value = value##nullableOfNullable;
            switch (Js.toOption(value)) {
            | Some(value) =>
              Some(
                value
                |> Js.Array.map(value =>
                     switch (Js.toOption(value)) {
                     | Some(value) => Some(value)
                     | None => None
                     }
                   ),
              )
            | None => None
            };
          },
          "nullableOfNonNullable": {
            let value = value##nullableOfNonNullable;
            switch (Js.toOption(value)) {
            | Some(value) => Some(value |> Js.Array.map(value => value))
            | None => None
            };
          },
          "nonNullableOfNullable": {
            let value = value##nonNullableOfNullable;
            value
            |> Js.Array.map(value =>
                 switch (Js.toOption(value)) {
                 | Some(value) => Some(value)
                 | None => None
                 }
               );
          },
          "nonNullableOfNonNullable": {
            let value = value##nonNullableOfNonNullable;
            value |> Js.Array.map(value => value);
          },
        };
      },
    };
  let serialize: t => Raw.t =
    value => {
      let lists = {
        let value = value##lists;
        let nonNullableOfNonNullable = {
          let value = value##nonNullableOfNonNullable;

          value |> Js.Array.map(value => value);
        }
        and nonNullableOfNullable = {
          let value = value##nonNullableOfNullable;

          value
          |> Js.Array.map(value =>
               switch (value) {
               | Some(value) => Js.Nullable.return(value)
               | None => Js.Nullable.null
               }
             );
        }
        and nullableOfNonNullable = {
          let value = value##nullableOfNonNullable;

          switch (value) {
          | Some(value) =>
            Js.Nullable.return(value |> Js.Array.map(value => value))
          | None => Js.Nullable.null
          };
        }
        and nullableOfNullable = {
          let value = value##nullableOfNullable;

          switch (value) {
          | Some(value) =>
            Js.Nullable.return(
              value
              |> Js.Array.map(value =>
                   switch (value) {
                   | Some(value) => Js.Nullable.return(value)
                   | None => Js.Nullable.null
                   }
                 ),
            )
          | None => Js.Nullable.null
          };
        };
        {

          "nullableOfNullable": nullableOfNullable,

          "nullableOfNonNullable": nullableOfNonNullable,

          "nonNullableOfNullable": nonNullableOfNullable,

          "nonNullableOfNonNullable": nonNullableOfNonNullable,
        };
      };
      {

        "lists": lists,
      };
    };
  let make = () => {
    "query": query,
    "variables": Js.Json.null,
    "parse": parse,
  };
  let definition = (parse, query, serialize);
};

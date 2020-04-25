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
    type t = {. "customFields": t_customFields}
    and t_customFields = {
      .
      "currentTime": Js.Json.t,
      "favoriteColor": Js.Json.t,
      "futureTime": Js.Nullable.t(Js.Json.t),
      "nullableColor": Js.Nullable.t(Js.Json.t),
    };
  };
  let query = "query   {\ncustomFields  {\ncurrentTime  \nfavoriteColor  \nfutureTime  \nnullableColor  \n}\n\n}\n";
  type t = {. "customFields": t_customFields}
  and t_customFields = {
    .
    "currentTime": GraphqlHelpers.DateTime.t,
    "favoriteColor": GraphqlHelpers.Color.t,
    "futureTime": option(GraphqlHelpers.DateTime.t),
    "nullableColor": option(GraphqlHelpers.Color.t),
  };
  let parse: Raw.t => t =
    value => {

      "customFields": {
        let value = value##customFields;
        {

          "currentTime": {
            let value = value##currentTime;

            GraphqlHelpers.DateTime.parse(value);
          },

          "favoriteColor": {
            let value = value##favoriteColor;

            GraphqlHelpers.Color.parse(value);
          },

          "futureTime": {
            let value = value##futureTime;

            switch (Js.toOption(value)) {
            | Some(value) => Some(GraphqlHelpers.DateTime.parse(value))
            | None => None
            };
          },

          "nullableColor": {
            let value = value##nullableColor;

            switch (Js.toOption(value)) {
            | Some(value) => Some(GraphqlHelpers.Color.parse(value))
            | None => None
            };
          },
        };
      },
    };
  let makeVar = (~f, ()) => f(Js.Json.null);
  let definition = (parse, query, makeVar);
};

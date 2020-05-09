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
    type t_customFields = {
      .
      "currentTime": Js.Json.t,
      "favoriteColor": Js.Json.t,
      "futureTime": Js.Nullable.t(Js.Json.t),
      "nullableColor": Js.Nullable.t(Js.Json.t),
    };
    type t = {. "customFields": t_customFields};
  };
  let query = "query   {\ncustomFields  {\ncurrentTime  \nfavoriteColor  \nfutureTime  \nnullableColor  \n}\n\n}\n";
  type t_customFields = {
    .
    "currentTime": GraphqlHelpers.DateTime.t,
    "favoriteColor": GraphqlHelpers.Color.t,
    "futureTime": option(GraphqlHelpers.DateTime.t),
    "nullableColor": option(GraphqlHelpers.DateTime.t),
  };
  type t = {. "customFields": t_customFields};
  let parse: Raw.t => t =
    value => {
      let customFields = {
        let value = value##customFields;
        let nullableColor = {
          let value = value##nullableColor;
          switch (Js.toOption(value)) {
          | Some(value) =>
            Some(
              GraphqlHelpers.DateTime.parse(
                GraphqlHelpers.Color.parse(value),
              ),
            )
          | None => None
          };
        }
        and futureTime = {
          let value = value##futureTime;
          switch (Js.toOption(value)) {
          | Some(value) => Some(GraphqlHelpers.DateTime.parse(value))
          | None => None
          };
        }
        and favoriteColor = {
          let value = value##favoriteColor;
          GraphqlHelpers.Color.parse(value);
        }
        and currentTime = {
          let value = value##currentTime;
          GraphqlHelpers.DateTime.parse(value);
        };
        {
          "currentTime": currentTime,
          "favoriteColor": favoriteColor,
          "futureTime": futureTime,
          "nullableColor": nullableColor,
        };
      };
      {"customFields": customFields};
    };
  let serialize: t => Raw.t =
    value => {
      let customFields = {
        let value = value##customFields;
        let nullableColor = {
          let value = value##nullableColor;
          switch (value) {
          | Some(value) =>
            Js.Nullable.return(
              GraphqlHelpers.DateTime.serialize(
                GraphqlHelpers.Color.serialize(value),
              ),
            )
          | None => Js.Nullable.null
          };
        }
        and futureTime = {
          let value = value##futureTime;
          switch (value) {
          | Some(value) =>
            Js.Nullable.return(GraphqlHelpers.DateTime.serialize(value))
          | None => Js.Nullable.null
          };
        }
        and favoriteColor = {
          let value = value##favoriteColor;
          GraphqlHelpers.Color.serialize(value);
        }
        and currentTime = {
          let value = value##currentTime;
          GraphqlHelpers.DateTime.serialize(value);
        };
        {
          "currentTime": currentTime,
          "favoriteColor": favoriteColor,
          "futureTime": futureTime,
          "nullableColor": nullableColor,
        };
      };
      {"customFields": customFields};
    };
  let make = () => {
    "query": query,
    "variables": Js.Json.null,
    "parse": parse,
  };
  let definition = (parse, query, serialize);
  module Z__INTERNAL = {
    type root = t;
  };
};

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
      currentTime: Js.Json.t,
      favoriteColor: Js.Json.t,
      futureTime: Js.Nullable.t(Js.Json.t),
      nullableColor: Js.Nullable.t(Js.Json.t),
    };
    type t = {customFields: t_customFields};
  };
  let query = "query   {\ncustomFields  {\ncurrentTime  \nfavoriteColor  \nfutureTime  \nnullableColor  \n}\n\n}\n";
  type t_customFields = {
    currentTime: GraphqlHelpers.DateTime.t,
    favoriteColor: GraphqlHelpers.Color.t,
    futureTime: option(GraphqlHelpers.DateTime.t),
    nullableColor: option(GraphqlHelpers.DateTime.t),
  };
  type t = {customFields: t_customFields};
  type operation = t;
  let parse: Raw.t => t =
    (value) => (
      {
        let customFields = {
          let value = (value: Raw.t).customFields;
          (
            {
              let nullableColor = {
                let value = (value: Raw.t_customFields).nullableColor;
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
                let value = (value: Raw.t_customFields).futureTime;
                switch (Js.toOption(value)) {
                | Some(value) => Some(GraphqlHelpers.DateTime.parse(value))
                | None => None
                };
              }
              and favoriteColor = {
                let value = (value: Raw.t_customFields).favoriteColor;
                GraphqlHelpers.Color.parse(value);
              }
              and currentTime = {
                let value = (value: Raw.t_customFields).currentTime;
                GraphqlHelpers.DateTime.parse(value);
              };
              {currentTime, favoriteColor, futureTime, nullableColor};
            }: t_customFields
          );
        };
        {customFields: customFields};
      }: t
    );
  let serialize: t => Raw.t =
    (value) => (
      {
        let customFields = {
          let value = (value: t).customFields;
          (
            {
              let nullableColor = {
                let value = (value: t_customFields).nullableColor;
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
                let value = (value: t_customFields).futureTime;
                switch (value) {
                | Some(value) =>
                  Js.Nullable.return(
                    GraphqlHelpers.DateTime.serialize(value),
                  )
                | None => Js.Nullable.null
                };
              }
              and favoriteColor = {
                let value = (value: t_customFields).favoriteColor;
                GraphqlHelpers.Color.serialize(value);
              }
              and currentTime = {
                let value = (value: t_customFields).currentTime;
                GraphqlHelpers.DateTime.serialize(value);
              };
              {currentTime, favoriteColor, futureTime, nullableColor};
            }: Raw.t_customFields
          );
        };
        {customFields: customFields};
      }: Raw.t
    );
  let definition = (parse, query, serialize);
};

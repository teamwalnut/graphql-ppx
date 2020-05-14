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
module Color = {
  type t =
    | Red
    | Green
    | Blue;
  let parse = json =>
    switch (json |> Js.Json.decodeString) {
    | Some("green") => Green
    | Some("blue") => Blue
    | Some("red")
    | Some(_)
    | None => Red
    };
  let serialize = color =>
    (
      switch (color) {
      | Red => "red"
      | Green => "green"
      | Blue => "blue"
      }
    )
    |> Js.Json.string;
};
module DateTime = {
  type t = Js.Date.t;
  let parse = json =>
    switch (json |> Js.Json.decodeString) {
    | Some(str) => str |> Js.Date.fromString
    | None => Js.Date.make()
    };
  let serialize = date => date |> Js.Date.toISOString |> Js.Json.string;
};

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
    currentTime: DateTime.t,
    favoriteColor: Color.t,
    futureTime: option(DateTime.t),
    nullableColor: option(Color.t),
  };
  type t = {customFields: t_customFields};
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
                | Some(value) => Some(Color.parse(Color.parse(value)))
                | None => None
                };
              }
              and futureTime = {
                let value = (value: Raw.t_customFields).futureTime;
                switch (Js.toOption(value)) {
                | Some(value) => Some(DateTime.parse(value))
                | None => None
                };
              }
              and favoriteColor = {
                let value = (value: Raw.t_customFields).favoriteColor;
                Color.parse(value);
              }
              and currentTime = {
                let value = (value: Raw.t_customFields).currentTime;
                DateTime.parse(value);
              };
              {

                currentTime,

                favoriteColor,

                futureTime,

                nullableColor,
              };
            }: t_customFields
          );
        };
        {

          customFields: customFields,
        };
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
                    Color.serialize(Color.serialize(value)),
                  )
                | None => Js.Nullable.null
                };
              }
              and futureTime = {
                let value = (value: t_customFields).futureTime;

                switch (value) {
                | Some(value) =>
                  Js.Nullable.return(DateTime.serialize(value))
                | None => Js.Nullable.null
                };
              }
              and favoriteColor = {
                let value = (value: t_customFields).favoriteColor;

                Color.serialize(value);
              }
              and currentTime = {
                let value = (value: t_customFields).currentTime;

                DateTime.serialize(value);
              };
              {

                currentTime,

                favoriteColor,

                futureTime,

                nullableColor,
              };
            }: Raw.t_customFields
          );
        };
        {

          customFields: customFields,
        };
      }: Raw.t
    );
  let definition = (parse, query, serialize);
};

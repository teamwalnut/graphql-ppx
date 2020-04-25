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
    type t_v1 = {
      .
      "nullableString": Js.Nullable.t(string),
      "string": Js.Nullable.t(string),
    };
    type t_v2 = {
      .
      "nullableString": Js.Nullable.t(string),
      "string": Js.Nullable.t(string),
    };
    type t = {
      .
      "v1": t_v1,
      "v2": t_v2,
    };
    type t_variables = {. "var": bool};
  };
  let query = "query ($var: Boolean!)  {\nv1: variousScalars  {\nnullableString @skip(if: $var) \nstring @skip(if: $var) \n}\n\nv2: variousScalars  {\nnullableString @include(if: $var) \nstring @include(if: $var) \n}\n\n}\n";
  type t_v1 = {
    .
    "nullableString": option(string),
    "string": option(string),
  };
  type t_v2 = {
    .
    "nullableString": option(string),
    "string": option(string),
  };
  type t = {
    .
    "v1": t_v1,
    "v2": t_v2,
  };
  type t_variables = {. "var": bool};
  let parse: Raw.t => t =
    value => {

      "v1": {
        let value = value##v1;
        {

          "nullableString": {
            let value = value##nullableString;

            switch (Js.toOption(value)) {
            | Some(value) => Some(value)
            | None => None
            };
          },

          "string": {
            let value = value##string;

            switch (Js.toOption(value)) {
            | Some(value) => Some(value)
            | None => None
            };
          },
        };
      },

      "v2": {
        let value = value##v2;
        {

          "nullableString": {
            let value = value##nullableString;

            switch (Js.toOption(value)) {
            | Some(value) => Some(value)
            | None => None
            };
          },

          "string": {
            let value = value##string;

            switch (Js.toOption(value)) {
            | Some(value) => Some(value)
            | None => None
            };
          },
        };
      },
    };
  let serialize: t => Raw.t =
    value => {
      let v2 = {
        let value = value##v2;
        let string = {
          let value = value##string;

          switch (value) {
          | Some(value) => Js.Nullable.return(value)
          | None => Js.Nullable.null
          };
        }
        and nullableString = {
          let value = value##nullableString;

          switch (value) {
          | Some(value) => Js.Nullable.return(value)
          | None => Js.Nullable.null
          };
        };
        {

          "nullableString": nullableString,

          "string": string,
        };
      }
      and v1 = {
        let value = value##v1;
        let string = {
          let value = value##string;

          switch (value) {
          | Some(value) => Js.Nullable.return(value)
          | None => Js.Nullable.null
          };
        }
        and nullableString = {
          let value = value##nullableString;

          switch (value) {
          | Some(value) => Js.Nullable.return(value)
          | None => Js.Nullable.null
          };
        };
        {

          "nullableString": nullableString,

          "string": string,
        };
      };
      {

        "v1": v1,

        "v2": v2,
      };
    };
  let serializeVariables: t_variables => Js.Json.t =
    inp =>
      [|("var", (a => Some(Js.Json.boolean(a)))(inp##var))|]
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
  let make = (~var, ()) => {
    "query": query,
    "variables":
      serializeVariables(
        {

          "var": var,
        }: t_variables,
      ),
    "parse": parse,
  }
  and makeVariables = (~var, ()) =>
    serializeVariables(
      {

        "var": var,
      }: t_variables,
    );
  let makeWithVariables = variables => {
    "query": query,
    "variables": serializeVariables(variables),
    "parse": parse,
  };
  let definition = (parse, query, serialize);
};

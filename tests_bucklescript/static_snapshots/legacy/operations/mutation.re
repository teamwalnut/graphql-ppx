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
    type t = {. "mutationWithError": t_mutationWithError}
    and t_mutationWithError = {
      .
      "value": Js.Nullable.t(t_mutationWithError_value),
      "errors": Js.Nullable.t(array(t_mutationWithError_errors)),
    }
    and t_mutationWithError_errors = {
      .
      "field": t_mutationWithError_errors_field,
      "message": string,
    }
    and t_mutationWithError_errors_field = string
    and t_mutationWithError_value = {. "stringField": string};
  };
  let query = "mutation   {\nmutationWithError  {\nvalue  {\nstringField  \n}\n\nerrors  {\nfield  \nmessage  \n}\n\n}\n\n}\n";
  type t = {. "mutationWithError": t_mutationWithError}
  and t_mutationWithError = {
    .
    "value": option(t_mutationWithError_value),
    "errors": option(array(t_mutationWithError_errors)),
  }
  and t_mutationWithError_errors = {
    .
    "field": t_mutationWithError_errors_field,
    "message": string,
  }
  and t_mutationWithError_errors_field = [
    | `FutureAddedValue(string)
    | `FIRST
    | `SECOND
    | `THIRD
  ]
  and t_mutationWithError_value = {. "stringField": string};
  let parse: Raw.t => t =
    value => {

      "mutationWithError": {
        let value = value##mutationWithError;
        {

          "value": {
            let value = value##value;

            switch (Js.toOption(value)) {
            | Some(value) =>
              Some({

                "stringField": {
                  let value = value##stringField;

                  value;
                },
              })
            | None => None
            };
          },

          "errors": {
            let value = value##errors;

            switch (Js.toOption(value)) {
            | Some(value) =>
              Some(
                value
                |> Js.Array.map(value =>
                     {

                       "field": {
                         let value = value##field;
                         switch (Obj.magic(value: string)) {
                         | "FIRST" => `FIRST
                         | "SECOND" => `SECOND
                         | "THIRD" => `THIRD
                         | other => `FutureAddedValue(other)
                         };
                       },

                       "message": {
                         let value = value##message;

                         value;
                       },
                     }
                   ),
              )
            | None => None
            };
          },
        };
      },
    };
  let serialize: t => Raw.t =
    value => {
      let mutationWithError = {
        let value = value##mutationWithError;
        let errors = {
          let value = value##errors;

          switch (value) {
          | Some(value) =>
            Js.Nullable.return(
              value
              |> Js.Array.map(value =>
                   let message = {
                     let value = value##message;

                     value;
                   }
                   and field = {
                     let value = value##field;
                     switch (value) {
                     | `FIRST => "FIRST"
                     | `SECOND => "SECOND"
                     | `THIRD => "THIRD"
                     | `FutureAddedValue(other) => other
                     };
                   };
                   {

                     "field": field,

                     "message": message,
                   };
                 ),
            )
          | None => Js.Nullable.null
          };
        }
        and value = {
          let value = value##value;

          switch (value) {
          | Some(value) =>
            Js.Nullable.return(
              {
                let stringField = {
                  let value = value##stringField;

                  value;
                };
                {

                  "stringField": stringField,
                };
              },
            )
          | None => Js.Nullable.null
          };
        };
        {

          "value": value,

          "errors": errors,
        };
      };
      {

        "mutationWithError": mutationWithError,
      };
    };
  let makeWithVariables = variables => {
    "query": query,
    "variables": serializeVariables(variables),
    "parse": parse,
  };
  let make = () => {
    "query": query,
    "variables": Js.Json.null,
    "parse": parse,
  };
  let definition = (parse, query, serialize);
};

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
    type t = {mutationWithError: t_mutationWithError}
    and t_mutationWithError
    and t_mutationWithError_errors = {
      field: t_mutationWithError_errors_field,
      message: string,
    }
    and t_mutationWithError_errors_field = string
    and t_mutationWithError_value = {stringField: string};
  };
  let query = "mutation   {\nmutationWithError  {\nvalue  {\nstringField  \n}\n\nerrors  {\nfield  \nmessage  \n}\n\n}\n\n}\n";
  type t = {mutationWithError: t_mutationWithError}
  and t_mutationWithError = [
    | `Value(t_mutationWithError_value)
    | `Errors(array(t_mutationWithError_errors))
  ]
  and t_mutationWithError_errors = {
    field: t_mutationWithError_errors_field,
    message: string,
  }
  and t_mutationWithError_errors_field = [
    | `FutureAddedValue(string)
    | `FIRST
    | `SECOND
    | `THIRD
  ]
  and t_mutationWithError_value = {stringField: string};
  let parse: Raw.t => t =
    (value) => (
      {

        mutationWithError: {
          let value = (value: Raw.t).mutationWithError;

          switch (Js.Json.decodeObject(Obj.magic(value): Js.Json.t)) {

          | None =>
            Js.Exn.raiseError(
              "graphql_ppx: "
              ++ "Expected type "
              ++ "MutationWithErrorResult"
              ++ " to be an object",
            )

          | Some(value) =>
            let temp = Js.Dict.unsafeGet(Obj.magic(value), "value");
            switch (Js.Json.decodeNull(temp)) {
            | None =>
              let value = temp;
              `Value(
                {

                  stringField: {
                    let value =
                      (value: Raw.t_mutationWithError_value).stringField;

                    value;
                  },
                }: t_mutationWithError_value,
              );
            | Some(_) =>
              let temp = Js.Dict.unsafeGet(Obj.magic(value), "errors");
              switch (Js.Json.decodeNull(temp)) {
              | None =>
                let value = temp;
                `Errors(
                  value
                  |> Js.Array.map((value) =>
                       (
                         {

                           field: {
                             let value =
                               (value: Raw.t_mutationWithError_errors).field;
                             switch (Obj.magic(value: string)) {
                             | "FIRST" => `FIRST
                             | "SECOND" => `SECOND
                             | "THIRD" => `THIRD
                             | other => `FutureAddedValue(other)
                             };
                           },

                           message: {
                             let value =
                               (value: Raw.t_mutationWithError_errors).message;

                             value;
                           },
                         }: t_mutationWithError_errors
                       )
                     ),
                );
              | Some(_) =>
                Js.Exn.raiseError(
                  "graphql_ppx: "
                  ++ "All fields on variant selection set on type "
                  ++ "MutationWithErrorResult"
                  ++ " were null",
                )
              };
            };
          };
        },
      }: t
    );
  let makeVar = (~f, ()) => f(Js.Json.null);
  let definition = (parse, query, makeVar);
};

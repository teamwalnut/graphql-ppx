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
module Normal = {
  module Raw = {
    type t_mutationWithError_errors_field = string;
    type t_mutationWithError_errors = {
      .
      "message": string,
      "field": t_mutationWithError_errors_field,
    };
    type t_mutationWithError = {
      .
      "errors": Js.Nullable.t(array(t_mutationWithError_errors)),
    };
    type t = {. "mutationWithError": t_mutationWithError};
  };
  let query = "mutation   {\nmutationWithError  {\nerrors  {\nmessage  \nfield  \n}\n\n}\n\n}\n";
  type t_mutationWithError_errors_field = [
    | `FutureAddedValue(string)
    | `FIRST
    | `SECOND
    | `THIRD
  ];
  type t_mutationWithError_errors = {
    .
    "message": string,
    "field": t_mutationWithError_errors_field,
  };
  type t_mutationWithError = {
    .
    "errors": option(array(t_mutationWithError_errors)),
  };
  type t = {. "mutationWithError": t_mutationWithError};
  type operation = t;
  let parse: Raw.t => t =
    value => {
      let mutationWithError = {
        let value = value##mutationWithError;
        let errors = {
          let value = value##errors;
          switch (Js.toOption(value)) {
          | Some(value) =>
            Some(
              value
              |> Js.Array.map(value =>
                   let field = {
                     let value = value##field;
                     switch (Obj.magic(value: string)) {
                     | "FIRST" => `FIRST
                     | "SECOND" => `SECOND
                     | "THIRD" => `THIRD
                     | other => `FutureAddedValue(other)
                     };
                   }
                   and message = {
                     let value = value##message;
                     value;
                   };
                   {"message": message, "field": field};
                 ),
            )
          | None => None
          };
        };
        {"errors": errors};
      };
      {"mutationWithError": mutationWithError};
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
                   let field = {
                     let value = value##field;
                     switch (value) {
                     | `FIRST => "FIRST"
                     | `SECOND => "SECOND"
                     | `THIRD => "THIRD"
                     | `FutureAddedValue(other) => other
                     };
                   }
                   and message = {
                     let value = value##message;
                     value;
                   };
                   {"message": message, "field": field};
                 ),
            )
          | None => Js.Nullable.null
          };
        };
        {"errors": errors};
      };
      {"mutationWithError": mutationWithError};
    };
  let definition = (parse, query, serialize);
};
module ByConfig = {
  module Raw = {
    type t_mutationWithError_errors_field = string;
    type t_mutationWithError_errors = {
      .
      "message": string,
      "field": t_mutationWithError_errors_field,
    };
    type t_mutationWithError = {
      .
      "errors": Js.Nullable.t(array(t_mutationWithError_errors)),
    };
    type t = {. "mutationWithError": t_mutationWithError};
  };
  let query = "mutation   {\nmutationWithError  {\nerrors  {\nmessage  \nfield  \n}\n\n}\n\n}\n";
  type t_mutationWithError_errors_field = [ | `FIRST | `SECOND | `THIRD];
  type t_mutationWithError_errors = {
    .
    "message": string,
    "field": t_mutationWithError_errors_field,
  };
  type t_mutationWithError = {
    .
    "errors": option(array(t_mutationWithError_errors)),
  };
  type t = {. "mutationWithError": t_mutationWithError};
  type operation = t;
  let parse: Raw.t => t =
    value => {
      let mutationWithError = {
        let value = value##mutationWithError;
        let errors = {
          let value = value##errors;
          switch (Js.toOption(value)) {
          | Some(value) =>
            Some(
              value
              |> Js.Array.map(value =>
                   let field = {
                     let value = value##field;
                     switch (Obj.magic(value: string)) {
                     | "FIRST" => `FIRST
                     | "SECOND" => `SECOND
                     | "THIRD" => `THIRD
                     | _ => raise(Not_found)
                     };
                   }
                   and message = {
                     let value = value##message;
                     value;
                   };
                   {"message": message, "field": field};
                 ),
            )
          | None => None
          };
        };
        {"errors": errors};
      };
      {"mutationWithError": mutationWithError};
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
                   let field = {
                     let value = value##field;
                     switch (value) {
                     | `FIRST => "FIRST"
                     | `SECOND => "SECOND"
                     | `THIRD => "THIRD"
                     };
                   }
                   and message = {
                     let value = value##message;
                     value;
                   };
                   {"message": message, "field": field};
                 ),
            )
          | None => Js.Nullable.null
          };
        };
        {"errors": errors};
      };
      {"mutationWithError": mutationWithError};
    };
  let definition = (parse, query, serialize);
};
module ByDirective = {
  module Raw = {
    type t_mutationWithError_errors_field = string;
    type t_mutationWithError_errors = {
      .
      "message": string,
      "field": t_mutationWithError_errors_field,
    };
    type t_mutationWithError = {
      .
      "errors": Js.Nullable.t(array(t_mutationWithError_errors)),
    };
    type t = {. "mutationWithError": t_mutationWithError};
  };
  let query = "mutation   {\nmutationWithError  {\nerrors  {\nmessage  \nfield @ppxOmitFutureValue \n}\n\n}\n\n}\n";
  type t_mutationWithError_errors_field = [ | `FIRST | `SECOND | `THIRD];
  type t_mutationWithError_errors = {
    .
    "message": string,
    "field": t_mutationWithError_errors_field,
  };
  type t_mutationWithError = {
    .
    "errors": option(array(t_mutationWithError_errors)),
  };
  type t = {. "mutationWithError": t_mutationWithError};
  type operation = t;
  let parse: Raw.t => t =
    value => {
      let mutationWithError = {
        let value = value##mutationWithError;
        let errors = {
          let value = value##errors;
          switch (Js.toOption(value)) {
          | Some(value) =>
            Some(
              value
              |> Js.Array.map(value =>
                   let field = {
                     let value = value##field;
                     switch (Obj.magic(value: string)) {
                     | "FIRST" => `FIRST
                     | "SECOND" => `SECOND
                     | "THIRD" => `THIRD
                     | _ => raise(Not_found)
                     };
                   }
                   and message = {
                     let value = value##message;
                     value;
                   };
                   {"message": message, "field": field};
                 ),
            )
          | None => None
          };
        };
        {"errors": errors};
      };
      {"mutationWithError": mutationWithError};
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
                   let field = {
                     let value = value##field;
                     switch (value) {
                     | `FIRST => "FIRST"
                     | `SECOND => "SECOND"
                     | `THIRD => "THIRD"
                     };
                   }
                   and message = {
                     let value = value##message;
                     value;
                   };
                   {"message": message, "field": field};
                 ),
            )
          | None => Js.Nullable.null
          };
        };
        {"errors": errors};
      };
      {"mutationWithError": mutationWithError};
    };
  let definition = (parse, query, serialize);
};

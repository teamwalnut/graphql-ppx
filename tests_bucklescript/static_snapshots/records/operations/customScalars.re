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
    type t_customScalarField = {
      nullable: Js.Nullable.t(Js.Json.t),
      nonNullable: Js.Json.t,
    };
    type t = {customScalarField: t_customScalarField};
    type t_variables = {
      opt: Js.Nullable.t(Js.Json.t),
      req: Js.Json.t,
    };
  };
  let query = "query ($opt: CustomScalar, $req: CustomScalar!)  {\ncustomScalarField(argOptional: $opt, argRequired: $req)  {\nnullable  \nnonNullable  \n}\n\n}\n";
  type t_customScalarField = {
    nullable: option(Js.Json.t),
    nonNullable: Js.Json.t,
  };
  type t = {customScalarField: t_customScalarField};
  type operation = t;
  type t_variables = {
    opt: option(Js.Json.t),
    req: Js.Json.t,
  };
  let parse: Raw.t => t =
    (value) => (
      {
        customScalarField: {
          let value = (value: Raw.t).customScalarField;
          (
            {
              nullable: {
                let value = (value: Raw.t_customScalarField).nullable;
                switch (Js.toOption(value)) {
                | Some(value) => Some(value)
                | None => None
                };
              },
              nonNullable: {
                let value = (value: Raw.t_customScalarField).nonNullable;
                value;
              },
            }: t_customScalarField
          );
        },
      }: t
    );
  let serialize: t => Raw.t =
    (value) => (
      {
        let customScalarField = {
          let value = (value: t).customScalarField;
          (
            {
              let nonNullable = {
                let value = (value: t_customScalarField).nonNullable;
                value;
              }
              and nullable = {
                let value = (value: t_customScalarField).nullable;
                switch (value) {
                | Some(value) => Js.Nullable.return(value)
                | None => Js.Nullable.null
                };
              };
              {nullable, nonNullable};
            }: Raw.t_customScalarField
          );
        };
        {customScalarField: customScalarField};
      }: Raw.t
    );
  let serializeVariables: t_variables => Raw.t_variables =
    inp => {
      opt:
        (
          a =>
            switch (a) {
            | None => Js.Nullable.undefined
            | Some(b) => Js.Nullable.return((a => a)(b))
            }
        )(
          (inp: t_variables).opt,
        ),
      req: (a => a)((inp: t_variables).req),
    };
  let makeVariables = (~opt=?, ~req, ()) =>
    serializeVariables({opt, req}: t_variables);
  let definition = (parse, query, serialize);
};

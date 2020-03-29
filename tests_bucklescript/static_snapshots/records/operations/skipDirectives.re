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
    type t = {
      v1: t_v1,
      v2: t_v2,
    }
    and t_v2 = {
      nullableString: Js.Nullable.t(string),
      string: Js.Nullable.t(string),
    }
    and t_v1 = {
      nullableString: Js.Nullable.t(string),
      string: Js.Nullable.t(string),
    };
  };
  let query = "query ($var: Boolean!)  {\nv1: variousScalars  {\nnullableString @skip(if: $var) \nstring @skip(if: $var) \n}\n\nv2: variousScalars  {\nnullableString @include(if: $var) \nstring @include(if: $var) \n}\n\n}\n";
  type t = {
    v1: t_v1,
    v2: t_v2,
  }
  and t_v2 = {
    nullableString: option(string),
    string: option(string),
  }
  and t_v1 = {
    nullableString: option(string),
    string: option(string),
  };
  type t_variables = {var: bool};
  let parse: Js.Json.t => t =
    (value) => (
      {

        v1: {
          let value = Js.Dict.unsafeGet(Obj.magic(value), "v1");
          (
            {

              nullableString: {
                let value =
                  Js.Dict.unsafeGet(Obj.magic(value), "nullableString");

                switch (Js.toOption(Obj.magic(value): Js.Nullable.t('a))) {
                | Some(_) => Some(Obj.magic(value): string)
                | None => None
                };
              },

              string: {
                let value = Js.Dict.unsafeGet(Obj.magic(value), "string");

                switch (Js.toOption(Obj.magic(value): Js.Nullable.t('a))) {
                | Some(_) => Some(Obj.magic(value): string)
                | None => None
                };
              },
            }: t_v1
          );
        },

        v2: {
          let value = Js.Dict.unsafeGet(Obj.magic(value), "v2");
          (
            {

              nullableString: {
                let value =
                  Js.Dict.unsafeGet(Obj.magic(value), "nullableString");

                switch (Js.toOption(Obj.magic(value): Js.Nullable.t('a))) {
                | Some(_) => Some(Obj.magic(value): string)
                | None => None
                };
              },

              string: {
                let value = Js.Dict.unsafeGet(Obj.magic(value), "string");

                switch (Js.toOption(Obj.magic(value): Js.Nullable.t('a))) {
                | Some(_) => Some(Obj.magic(value): string)
                | None => None
                };
              },
            }: t_v2
          );
        },
      }: t
    );
  let serializeVariables: t_variables => Js.Json.t =
    inp =>
      [|("var", (a => Some(Js.Json.boolean(a)))(inp.var))|]
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
  let makeVar = (~f, ~var, ()) =>
    f(
      serializeVariables(
        {

          var: var,
        }: t_variables,
      ),
    );
  let definition = (parse, query, makeVar);
  let makeVariables = makeVar(~f=f => f);
};

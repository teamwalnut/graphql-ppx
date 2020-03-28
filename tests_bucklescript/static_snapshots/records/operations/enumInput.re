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
  let query = "query ($arg: SampleField!)  {\nenumInput(arg: $arg)  \n}\n";
  type raw_t;
  type t = {enumInput: string};
  type t_variables = {arg: [ | `FIRST | `SECOND | `THIRD]};
  let parse: Js.Json.t => t =
    (value) => (
      {

        enumInput: {
          let value = Js.Dict.unsafeGet(Obj.magic(value), "enumInput");

          (Obj.magic(value): string);
        },
      }: t
    );
  let serializeVariables: t_variables => Js.Json.t =
    inp =>
      [|
        (
          "arg",
          (
            a =>
              Some(
                switch (a) {
                | `FIRST => Js.Json.string("FIRST")
                | `SECOND => Js.Json.string("SECOND")
                | `THIRD => Js.Json.string("THIRD")
                },
              )
          )(
            inp.arg,
          ),
        ),
      |]
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
  let makeVar = (~f, ~arg, ()) =>
    f(
      serializeVariables(
        {

          arg: arg,
        }: t_variables,
      ),
    );
  let definition = (parse, query, makeVar);
  let makeVariables = makeVar(~f=f => f);
};

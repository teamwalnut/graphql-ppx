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
module StringOfInt = {
  let parse = string_of_int;
  type t = string;
};
module IntOfString = {
  let parse = int_of_string;
  type t = int;
};

module MyQuery = {
  module Raw = {
    type t = {variousScalars: t_variousScalars}
    and t_variousScalars = {
      string: Js.Json.t,
      int: Js.Json.t,
    };
  };
  let query = "query   {\nvariousScalars  {\nstring  \nint  \n}\n\n}\n";
  type t = {variousScalars: t_variousScalars}
  and t_variousScalars = {
    string: IntOfString.t,
    int: StringOfInt.t,
  };
  let parse: Js.Json.t => t =
    (value) => (
      {

        variousScalars: {
          let value = Js.Dict.unsafeGet(Obj.magic(value), "variousScalars");
          (
            {

              string: {
                let value = Js.Dict.unsafeGet(Obj.magic(value), "string");

                IntOfString.parse(Obj.magic(value): string);
              },

              int: {
                let value = Js.Dict.unsafeGet(Obj.magic(value), "int");

                StringOfInt.parse(Obj.magic(value): int);
              },
            }: t_variousScalars
          );
        },
      }: t
    );
  let makeVar = (~f, ()) => f(Js.Json.null);
  let definition = (parse, query, makeVar);
};

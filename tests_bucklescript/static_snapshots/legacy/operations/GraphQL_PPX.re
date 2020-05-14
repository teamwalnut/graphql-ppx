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

let%private clone: Js.Dict.t('a) => Js.Dict.t('a) =
  a => Obj.magic(Js.Obj.assign(Obj.magic(Js.Obj.empty()), Obj.magic(a)));

let rec deepMerge = (json1: Js.Json.t, json2: Js.Json.t) =>
  switch (
    (
      Obj.magic(json1) == Js.null,
      Js_array2.isArray(json1),
      Js.typeof(json1) == "object",
    ),
    (
      Obj.magic(json2) == Js.null,
      Js_array2.isArray(json2),
      Js.typeof(json2) == "object",
    ),
  ) {
  | ((_, true, _), (_, true, _)) => (
      Obj.magic(
        Js.Array.mapi(
          (el1, idx) => {
            let el2 = Js.Array.unsafe_get(Obj.magic(json2), idx);

            Js.typeof(el2) == "object" ? deepMerge(el1, el2) : el2;
          },
          Obj.magic(json1),
        ),
      ): Js.Json.t
    )

  | ((false, false, true), (false, false, true)) =>
    let obj1 = clone(Obj.magic(json1));
    let obj2 = Obj.magic(json2);
    Js.Dict.keys(obj2)
    |> Js.Array.forEach(key =>
         let existingVal: Js.Json.t = Js.Dict.unsafeGet(obj1, key);
         let newVal: Js.Json.t = Js.Dict.unsafeGet(obj2, key);
         Js.Dict.set(
           obj1,
           key,
           Js.typeof(existingVal) != "object"
             ? newVal : Obj.magic(deepMerge(existingVal, newVal)),
         );
       );
    Obj.magic(obj1);

  | ((_, _, _), (_, _, _)) => json2
  };

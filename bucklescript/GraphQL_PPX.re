let rec deepMerge = (json1: Js.Json.t, json2: Js.Json.t) => {
  switch (Js.Json.decodeObject(json1), Js.Json.decodeObject(json2)) {
  | (Some(obj1), Some(obj2)) =>
    let clone: 'a => 'a = (
      a =>
        Obj.magic(Js.Obj.assign(Obj.magic(Js.Obj.empty()), Obj.magic(a)))
    );
    let obj1 = clone(obj1);
    Js.Dict.keys(obj2)
    |> Js.Array.forEach(key => {
         Js.Dict.set(
           obj1,
           key,
           switch (Js.Dict.get(obj1, key), Js.Dict.unsafeGet(obj2, key)) {
           | (None, value) => value
           | (Some(value1), value2) => Obj.magic(deepMerge(value1, value2))
           },
         )
       });
    Obj.magic(obj1);
  | (_, _) => json2
  };
};

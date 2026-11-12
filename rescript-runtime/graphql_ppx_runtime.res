let assign_typename: (
  JSON.t,
  string,
) => JSON.t = %raw(` (obj, typename) => { obj.__typename = typename; return obj } `)

%%private(
  let clone: Dict.t<'a> => Dict.t<'a> = a =>
    Obj.magic(Object.assign(Obj.magic(Object.make()), Obj.magic(a)))
)

let rec deepMerge = (json1: JSON.t, json2: JSON.t) =>
  switch (
    (json1 == JSON.Null, Array.isArray(json1), typeof(json1) == #object),
    (json2 == JSON.Null, Array.isArray(json2), typeof(json2) == #object),
  ) {
  | ((_, true, _), (_, true, _)) =>
    (
      Obj.magic(
        Array.mapWithIndex(Obj.magic(json1), (el1, idx) => {
          let el2 = Array.getUnsafe(Obj.magic(json2), idx)
          typeof(el2) == #object ? deepMerge(el1, el2) : el2
        }),
      ): JSON.t
    )

  | ((false, false, true), (false, false, true)) =>
    let obj1 = clone(Obj.magic(json1))
    let obj2 = Obj.magic(json2)
    Array.forEach(Dict.keysToArray(obj2), key => {
      let existingVal: JSON.t = Dict.getUnsafe(obj1, key)
      let newVal: JSON.t = Dict.getUnsafe(obj2, key)
      Dict.set(
        obj1,
        key,
        typeof(existingVal) != #object ? newVal : Obj.magic(deepMerge(existingVal, newVal)),
      )
    })
    Obj.magic(obj1)
  | ((_, _, _), (_, _, _)) => json2
  }

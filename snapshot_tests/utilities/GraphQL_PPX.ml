let assign_typename : JSON.t -> string -> JSON.t =
  [%raw {| (obj, typename) => { obj.__typename = typename; return obj } |}]

[%%private
let clone =
  (fun a ->
     Obj.magic (Js.Obj.assign (Obj.magic (Js.Obj.empty ())) (Obj.magic a))
    : 'a Dict.t -> 'a Dict.t)]

let rec deepMerge (json1 : JSON.t) (json2 : JSON.t) =
  match
    ( (json1 = JSON.Null, Array.isArray json1, typeof json1 = "object"),
      (json2 = JSON.Null, Array.isArray json2, typeof json2 = "object") )
  with
  | (_, true, _), (_, true, _) ->
    (Obj.magic
       (Array.mapWithIndex (Obj.magic json1) (fun el1 idx ->
            let el2 = Array.getUnsafe (Obj.magic json2) idx in
            match typeof el2 = "object" with
            | true -> deepMerge el1 el2
            | false -> el2))
      : JSON.t)
  | (false, false, true), (false, false, true) ->
    let obj1 = clone (Obj.magic json1) in
    let obj2 = Obj.magic json2 in
    Array.forEach (Dict.keys obj2) (fun key ->
        let existingVal = (Dict.getUnsafe obj1 key : JSON.t) in
        let newVal = (Dict.getUnsafe obj2 key : JSON.t) in
        Dict.set obj1 key
          (match typeof existingVal <> "object" with
          | true -> newVal
          | false -> Obj.magic (deepMerge existingVal newVal)));
    Obj.magic obj1
  | (_, _, _), (_, _, _) -> json2

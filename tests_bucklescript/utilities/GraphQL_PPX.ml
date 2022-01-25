[%%private
  let clone =
    (fun a ->
       Obj.magic (Js.Obj.assign (Obj.magic (Js.Obj.empty ())) (Obj.magic a)) :
    'a Js.Dict.t -> 'a Js.Dict.t)]
let rec deepMerge (json1 : Js.Json.t) (json2 : Js.Json.t) =
  ((match ((((Obj.magic json1) = Js.null), (Js_array2.isArray json1),
             ((Js.typeof json1) = (("object")))),
            (((Obj.magic json2) = Js.null), (Js_array2.isArray json2),
              ((Js.typeof json2) = (("object")))))
    with
    | ((_, true, _), (_, true, _)) ->
        (Obj.magic
           (Js.Array.mapi
              (fun el1 ->
                 fun idx ->
                   ((let el2 = Js.Array.unsafe_get (Obj.magic json2) idx in
                     match (Js.typeof el2) = (("object")
                             )
                     with
                     | true -> deepMerge el1 el2
                     | false -> el2)
                   )) (Obj.magic json1)) :
        Js.Json.t)
    | ((false, false, true), (false, false, true)) ->
        let obj1 = clone (Obj.magic json1) in
        let obj2 = Obj.magic json2 in
        ((Js.Dict.keys obj2) |>
           (Js.Array.forEach
              (fun key ->
                 ((let existingVal = (Js.Dict.unsafeGet obj1 key : Js.Json.t) in
                   let newVal = (Js.Dict.unsafeGet obj2 key : Js.Json.t) in
                   Js.Dict.set obj1 key
                     (match (Js.typeof existingVal) <> (("object")
                              )
                      with
                      | true -> newVal
                      | false -> Obj.magic (deepMerge existingVal newVal)))
                 )));
         Obj.magic obj1)
    | ((_, _, _), (_, _, _)) -> json2)
  )

module RemoveFamilyMemberMutation =
  struct
    let query =
      "mutation RemoveFamilyMember($id: ID!)  {\nremoveFamilyMember(id: $id)  {\nid  \nmembers  {\nid  \n}\n\n}\n\n}\n"
    type raw_t
    type t = {
      removeFamilyMember: t_removeFamilyMember option }
    and t_removeFamilyMember =
      {
      id: string ;
      members: t_removeFamilyMember_members array }
    and t_removeFamilyMember_members = {
      id: string }
    type t_variables = {
      id: string }
    let parse =
      (fun value ->
         {
           removeFamilyMember =
             (let value =
                Js.Dict.unsafeGet (Obj.magic value) "removeFamilyMember" in
              match Js.toOption (Obj.magic value : 'a Js.Nullable.t) with
              | Some _ ->
                  ((Some
                      ({
                         id =
                           (let value =
                              Js.Dict.unsafeGet (Obj.magic value) "id" in
                            (Obj.magic value : string));
                         members =
                           (let value =
                              Js.Dict.unsafeGet (Obj.magic value) "members" in
                            (Obj.magic value) |>
                              (Js.Array.map
                                 (fun value ->
                                    {
                                      id =
                                        (let value =
                                           Js.Dict.unsafeGet
                                             (Obj.magic value) "id" in
                                         (Obj.magic value : string))
                                    })))
                       }))
                  [@explicit_arity ])
              | None -> None)
         } : Js.Json.t -> t)
    let (serializeVariables : t_variables -> Js.Json.t) =
      fun inp ->
        ((([|("id",
               (((fun a -> ((Some ((Js.Json.string a)))[@explicit_arity ])))
                  inp.id))|]
             |>
             (Js.Array.filter
                (function | (_, None) -> false | (_, Some _) -> true)))
            |>
            (Js.Array.map
               (function
                | (k, ((Some (v))[@explicit_arity ])) -> (k, v)
                | (k, None) -> (k, Js.Json.null))))
           |> Js.Dict.fromArray)
          |> Js.Json.object_
    let makeVar ~f  ~id  () = f (serializeVariables ({ id } : t_variables))
    let makeVariables = makeVar ~f:(fun f -> f)
    let make =
      makeVar ~f:(fun variables -> [%bs.obj { query; variables; parse }])
    let makeWithVariables variables =
      [%bs.obj { query; variables = (serializeVariables variables); parse }]
    let definition = (parse, query, makeVar)
  end

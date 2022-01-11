let option_map f o = match o with Some x -> Some (f x) | None -> None

let rec merge_pairs (pairs1 : (string * Yojson.Basic.t) list)
    (pairs2 : (string * Yojson.Basic.t) list) =
  let unique_keys =
    List.append
      (List.map (fun (k, _) -> k) pairs1)
      (List.map (fun (k, _) -> k) pairs2)
    |> List.fold_left
         (fun unique_keys key ->
           if List.exists (fun k -> k = key) unique_keys then key :: unique_keys
           else unique_keys)
         []
  in
  unique_keys
  |> List.map (fun key ->
         ( key,
           match
             ( List.find_opt (fun (assoc_key, _) -> key = assoc_key) pairs1
               |> option_map (fun pair -> snd pair),
               List.find_opt (fun (assoc_key, _) -> key = assoc_key) pairs2
               |> option_map (fun pair -> snd pair) )
           with
           | Some (`Assoc pairs1), Some (`Assoc pairs2) ->
               `Assoc (merge_pairs pairs1 pairs2)
           | _, Some (`Assoc pairs2) -> `Assoc pairs2
           | _, Some any -> any
           | Some any, _ -> any
           | None, None -> assert false ))

and deepMerge (json1 : Yojson.Basic.t) (json2 : Yojson.Basic.t) =
  match (json1, json2) with
  | `Assoc pairs1, `Assoc pairs2 -> `Assoc (merge_pairs pairs1 pairs2)
  | _, `Bool value -> `Bool value
  | _, `Float value -> `Float value
  | _, `String value -> `String value
  | _, `List values -> `List values
  | _, `Int value -> `Int value
  | _, `Null -> `Null
  | _, _ -> assert false

let option_map f o = match o with Some x -> Some (f x) | None -> None

module Json = struct
  type t =
    [ `Null
    | `Bool of bool
    | `Int of int
    | `Float of float
    | `String of string
    | `Assoc of (string * t) list
    | `List of t list ]

  exception Type_error of string * t

  let typeof = function
    | `Assoc _ -> "object"
    | `Bool _ -> "bool"
    | `Float _ -> "float"
    | `Int _ -> "int"
    | `List _ -> "array"
    | `Null -> "null"
    | `String _ -> "string"

  let typerr msg js = raise (Type_error (msg ^ typeof js, js))
  let assoc name obj = try List.assoc name obj with Not_found -> `Null

  let member name = function
    | `Assoc obj -> assoc name obj
    | js -> typerr ("Can't get member '" ^ name ^ "' of non-object type ") js

  let to_string = function
    | `String s -> s
    | js -> typerr "Expected string, got " js

  let to_float = function
    | `Float f -> f
    | js -> typerr "Expected float, got " js

  let to_int = function `Int i -> i | js -> typerr "Expected int, got " js
  let to_bool = function `Bool b -> b | js -> typerr "Expected bool, got " js
end

let rec merge_pairs (pairs1 : (string * Json.t) list)
  (pairs2 : (string * Json.t) list) =
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

and deepMerge (json1 : Json.t) (json2 : Json.t) =
  match (json1, json2) with
  | `Assoc pairs1, `Assoc pairs2 -> `Assoc (merge_pairs pairs1 pairs2)
  | _, `Bool value -> `Bool value
  | _, `Float value -> `Float value
  | _, `String value -> `String value
  | _, `List values -> `List values
  | _, `Int value -> `Int value
  | _, `Null -> `Null
  | _, _ -> assert false

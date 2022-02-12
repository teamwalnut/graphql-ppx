type t =
  [ `Null
  | `Bool of bool
  | `Int of int
  | `Float of float
  | `String of string
  | `Assoc of (string * t) list
  | `List of t list ]

(* Common *)
exception Json_error of string

let json_error s = raise (Json_error s)

exception End_of_array
exception End_of_object
exception End_of_tuple
exception End_of_input

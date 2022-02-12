open Json_type

let pp_list sep ppx out l =
  let pp_sep out () = Format.fprintf out "%s@ " sep in
  Format.pp_print_list ~pp_sep ppx out l

let rec format std (out : Format.formatter) (x : t) : unit =
  match x with
  | `Null -> Format.pp_print_string out "null"
  | `Bool x -> Format.pp_print_bool out x
  | `Int x -> Format.pp_print_string out (Json_write.json_string_of_int x)
  | `Float x ->
    let s =
      if std then Json_write.std_json_string_of_float x
      else Json_write.json_string_of_float x
    in
    Format.pp_print_string out s
  | `String s -> Format.pp_print_string out (Json_write.json_string_of_string s)
  | `List [] -> Format.pp_print_string out "[]"
  | `List l ->
    Format.fprintf out "[@;<1 0>@[<hov>%a@]@;<1 -2>]"
      (pp_list "," (format std))
      l
  | `Assoc [] -> Format.pp_print_string out "{}"
  | `Assoc l ->
    Format.fprintf out "{@;<1 0>%a@;<1 -2>}" (pp_list "," (format_field std)) l

and format_field std out (name, x) =
  Format.fprintf out "@[<hv2>%s: %a@]"
    (Json_write.json_string_of_string name)
    (format std) x

let pp ?(std = false) out x =
  Format.fprintf out "@[<hv2>%a@]" (format std) (x :> t)

let to_string ?std x = Format.asprintf "%a" (pp ?std) x

let to_channel ?std oc x =
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "%a@?" (pp ?std) x

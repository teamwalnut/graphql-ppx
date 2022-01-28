let yojson =
  (module struct
    type t = Yojson.Basic.t

    let pp formatter t =
      Format.pp_print_text formatter (Yojson.Basic.pretty_to_string t)

    let equal = ( = )
  end : Alcotest.TESTABLE
    with type t = Yojson.Basic.t)

let test_json a b =
  Alcotest.check yojson ("JSON equality" [@reason.raw_literal "JSON equality"])
    a b

let print_option inner formatter = function
  | None ->
    Format.pp_print_string formatter ("None" [@reason.raw_literal "None"])
  | ((Some v) [@explicit_arity]) ->
    Format.fprintf formatter
      ("Some(@[%a@])" [@reason.raw_literal "Some(@[%a@])"]) inner v

let print_array inner formatter value =
  ((let open Format in
   pp_print_string formatter ("[ " [@reason.raw_literal "[ "]);
   Array.iteri
     (fun idx v ->
       ((if idx > 0 then
           pp_print_string formatter ("; " [@reason.raw_literal "; "])
           [@reason.preserve_braces];
         pp_open_hovbox formatter 1;
         inner formatter v;
         pp_close_box formatter ())
       [@reason.preserve_braces]))
     value;
   pp_print_string formatter (" ]" [@reason.raw_literal " ]"]))
  [@reason.preserve_braces])

let array_zipmap f a b =
  (let min = min (Array.length a) (Array.length b) in
   Array.init min (fun i -> f a.(i) b.(i)))
  [@reason.preserve_braces]

let opt_eq f a b =
  match (a, b) with
  | ((Some a) [@explicit_arity]), ((Some b) [@explicit_arity]) -> f a b
  | None, None -> true
  | _ -> false

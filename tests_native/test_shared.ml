type result =
  | Pass
  | Fail of
      (Format.formatter -> unit -> unit) * (Format.formatter -> unit -> unit)

let make_unit_pp pp v fmt () = pp fmt v

let test_exp a b exp (pp : Format.formatter -> 'a -> unit) =
  if exp a b then Pass else Fail ((make_unit_pp pp) a, (make_unit_pp pp) b)

let pp_string = Format.pp_print_text
let test_str s1 s2 = test_exp s1 s2 ( = ) pp_string
let test_str_not s1 s2 = test_exp s1 s2 ( != ) pp_string

let yojson =
  (module struct
    type t = Yojson.Basic.t

    let pp formatter t =
      Format.pp_print_text formatter (Yojson.Basic.pretty_to_string t)

    let equal = ( = )
  end : Alcotest.TESTABLE
    with type t = Yojson.Basic.t)

let test_json a b = Alcotest.check yojson "JSON equality" a b

let pp_json formatter json =
  Format.pp_print_text formatter (Yojson.Basic.pretty_to_string json)

let json_equal a b = Yojson.Basic.equal a b
let test_json_ a b = test_exp a b json_equal pp_json

let print_option inner formatter = function
  | None -> Format.pp_print_string formatter "None"
  | Some v -> Format.fprintf formatter "Some(@[%a@])" inner v

let print_array inner formatter value =
  let open Format in
  pp_print_string formatter "[ ";
  Array.iteri
    (fun idx v ->
      if idx > 0 then pp_print_string formatter "; ";
      pp_open_hovbox formatter 1;
      inner formatter v;
      pp_close_box formatter ())
    value;
  pp_print_string formatter " ]"

let test_exp_array a b exp pp =
  test_exp a b
    (fun a b -> Array.for_all (fun (a, b) -> exp a b) (Array.combine a b))
    (print_array pp)

let array_zipmap f a b =
  let min = min (Array.length a) (Array.length b) in
  Array.init min (fun i -> f a.(i) b.(i))

let opt_eq f a b =
  match (a, b) with Some a, Some b -> f a b | None, None -> true | _ -> false

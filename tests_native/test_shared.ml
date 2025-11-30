type result =
  | Pass
  | CompareFail of
      (Format.formatter -> unit -> unit) * (Format.formatter -> unit -> unit)
  | Fail of string

let make_unit_pp pp v fmt () = pp fmt v

let test_exp a b exp (pp : Format.formatter -> 'a -> unit) =
  if exp a b then Pass
  else CompareFail ((make_unit_pp pp) a, (make_unit_pp pp) b)

let pp_string = Format.pp_print_text
let test_str s1 s2 = test_exp s1 s2 ( = ) pp_string
let test_str_not s1 s2 = test_exp s1 s2 ( != ) pp_string

let pp_json formatter json =
  Format.pp_print_text formatter (Json.pretty_to_string json)

let json_equal a b = Json.equal a b
let test_json_ a b = test_exp a b json_equal pp_json
let pp_color fmt color = Format.fprintf fmt "@<0>%s" color

let pp_colored pp fmt v color =
  pp_color fmt color;
  pp fmt v;
  pp_color fmt Cli_colors.reset

let pp_colored_str = pp_colored Format.pp_print_string

let pp_option inner formatter = function
  | None -> pp_colored_str formatter "None" Cli_colors.red
  | Some v ->
    pp_colored_str formatter "Some" Cli_colors.red;
    Format.fprintf formatter "(@[%a@])" inner v

let pp_array inner fmt value =
  let open Format in
  pp_colored_str fmt "[|" Cli_colors.blue;
  Format.pp_print_break fmt 1 2;
  Format.pp_open_hvbox fmt 0;
  Array.iteri
    (fun idx v ->
      pp_open_hvbox fmt 0;
      inner fmt v;
      pp_close_box fmt ();
      if idx < Array.length value - 1 then (
        pp_colored_str fmt ";" Cli_colors.blue;
        pp_print_break fmt 1 2)
      else (
        pp_color fmt Cli_colors.blue;
        Format.pp_print_if_newline fmt ();
        Format.pp_print_string fmt ";";
        pp_color fmt Cli_colors.reset))
    value;
  Format.pp_close_box fmt ();
  Format.pp_print_break fmt 1 0;
  pp_colored_str fmt "|]" Cli_colors.blue

let pp_record fmt (fields : (string * (Format.formatter -> unit)) list) =
  pp_colored_str fmt "{" Cli_colors.blue;
  Format.pp_print_break fmt 1 2;
  Format.pp_open_hvbox fmt 0;
  let len = List.length fields in
  fields
  |> List.iteri (fun i (field, value) ->
         Format.pp_open_hvbox fmt 0;
         pp_colored_str fmt field Cli_colors.purple;
         pp_colored_str fmt " = " Cli_colors.Dimmed.purple;
         value fmt;
         Format.pp_close_box fmt ();
         if i < len - 1 then (
           pp_colored_str fmt ";" Cli_colors.blue;
           Format.pp_print_break fmt 1 0)
         else (
           pp_color fmt Cli_colors.blue;
           Format.pp_print_if_newline fmt ();
           Format.pp_print_string fmt ";";
           pp_color fmt Cli_colors.reset));
  Format.pp_close_box fmt ();
  Format.pp_print_break fmt 1 0;
  pp_colored_str fmt "}" Cli_colors.blue

(* Vendored from Array.combine in stdlib, because it's just included in 4.13 *)
let array_combine a b =
  let na = Array.length a in
  let nb = Array.length b in
  if na <> nb then invalid_arg "Array.combine";
  if na = 0 then [||]
  else
    let x = Array.make na (Array.unsafe_get a 0, Array.unsafe_get b 0) in
    for i = 1 to na - 1 do
      Array.unsafe_set x i (Array.unsafe_get a i, Array.unsafe_get b i)
    done;
    x

let test_exp_array a b exp pp =
  test_exp a b
    (fun a b -> Array.for_all (fun (a, b) -> exp a b) (array_combine a b))
    (pp_array pp)

let array_zipmap f a b =
  let min = min (Array.length a) (Array.length b) in
  Array.init min (fun i -> f a.(i) b.(i))

let opt_eq f a b =
  match (a, b) with Some a, Some b -> f a b | None, None -> true | _ -> false

let pp_string_literal fmt s =
  pp_colored_str fmt ("\"" ^ s ^ "\"") Cli_colors.yellow

let swap_args fn fmt fields = fn fields fmt
let pp_to_print = swap_args
let print_record = pp_to_print pp_record
let print_option field inner fmt = pp_option (pp_to_print inner) fmt field
let print_string_literal = pp_to_print pp_string_literal

let print_int_literal integer fmt =
  pp_colored Format.pp_print_int fmt integer Cli_colors.yellow

let print_float_literal f fmt =
  pp_colored Format.pp_print_float fmt f Cli_colors.yellow

open Json_type
(* type in_param = { string_buf : Buffer.t } *)

(* let create_in_param ?(len = 256) () = { string_buf = Buffer.create len } *)

let utf8_of_code buf x =
  let add = Buffer.add_char in

  (* Straight <= doesn't work with signed 31-bit ints *)
  let maxbits n x = x lsr n = 0 in

  if maxbits 7 x then (* 7 *)
    add buf (Char.chr x)
  else if maxbits 11 x then (
    (* 5 + 6 *)
    add buf (Char.chr (0b11000000 lor ((x lsr 6) land 0b00011111)));
    add buf (Char.chr (0b10000000 lor (x land 0b00111111))))
  else if maxbits 16 x then (
    (* 4 + 6 + 6 *)
    add buf (Char.chr (0b11100000 lor ((x lsr 12) land 0b00001111)));
    add buf (Char.chr (0b10000000 lor ((x lsr 6) land 0b00111111)));
    add buf (Char.chr (0b10000000 lor (x land 0b00111111))))
  else if maxbits 21 x then (
    (* 3 + 6 + 6 + 6 *)
    add buf (Char.chr (0b11110000 lor ((x lsr 18) land 0b00000111)));
    add buf (Char.chr (0b10000000 lor ((x lsr 12) land 0b00111111)));
    add buf (Char.chr (0b10000000 lor ((x lsr 6) land 0b00111111)));
    add buf (Char.chr (0b10000000 lor (x land 0b00111111))))
  else if maxbits 26 x then (
    (* 2 + 6 + 6 + 6 + 6 *)
    add buf (Char.chr (0b11111000 lor ((x lsr 24) land 0b00000011)));
    add buf (Char.chr (0b10000000 lor ((x lsr 18) land 0b00111111)));
    add buf (Char.chr (0b10000000 lor ((x lsr 12) land 0b00111111)));
    add buf (Char.chr (0b10000000 lor ((x lsr 6) land 0b00111111)));
    add buf (Char.chr (0b10000000 lor (x land 0b00111111))))
  else (
    assert (maxbits 31 x);
    (* 1 + 6 + 6 + 6 + 6 + 6 *)
    add buf (Char.chr (0b11111100 lor ((x lsr 30) land 0b00000001)));
    add buf (Char.chr (0b10000000 lor ((x lsr 24) land 0b00111111)));
    add buf (Char.chr (0b10000000 lor ((x lsr 18) land 0b00111111)));
    add buf (Char.chr (0b10000000 lor ((x lsr 12) land 0b00111111)));
    add buf (Char.chr (0b10000000 lor ((x lsr 6) land 0b00111111)));
    add buf (Char.chr (0b10000000 lor (x land 0b00111111))))

let code_of_surrogate_pair i j =
  let high10 = i - 0xD800 in
  let low10 = j - 0xDC00 in
  0x10000 + ((high10 lsl 10) lor low10)

let utf8_of_surrogate_pair buf i j =
  utf8_of_code buf (code_of_surrogate_pair i j)

type lexer_state = {
  buf : Buffer.t; (* Buffer used to accumulate substrings *)
  mutable lnum : int; (* Current line number (starting from 1) *)
  mutable bol : int;
  (* Absolute position of the first character of the current line
     (starting from 0) *)
  mutable fname : string option; (* Name describing the input file *)
}

module Lexer_state = struct
  type t = lexer_state = {
    buf : Buffer.t;
    mutable lnum : int;
    mutable bol : int;
    mutable fname : string option;
  }
end

let init_lexer ?buf ?fname ?(lnum = 1) () =
  let buf = match buf with None -> Buffer.create 256 | Some buf -> buf in
  { buf; lnum; bol = 0; fname }

module Pretty = struct
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
    | `String s ->
      Format.pp_print_string out (Json_write.json_string_of_string s)
    | `List [] -> Format.pp_print_string out "[]"
    | `List l ->
      Format.fprintf out "[@;<1 0>@[<hov>%a@]@;<1 -2>]"
        (pp_list "," (format std))
        l
    | `Assoc [] -> Format.pp_print_string out "{}"
    | `Assoc l ->
      Format.fprintf out "{@;<1 0>%a@;<1 -2>}"
        (pp_list "," (format_field std))
        l

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
end

let pretty_print ?std out x = Pretty.pp ?std out x
let pretty_to_string ?std x = Pretty.to_string ?std x
let pretty_to_channel ?std oc x = Pretty.to_channel ?std oc x

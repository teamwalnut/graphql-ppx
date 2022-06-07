type t =
  [ `Null
  | `Bool of bool
  | `Int of int
  | `Float of float
  | `String of string
  | `Assoc of (string * t) list
  | `List of t list ]

exception Json_error of string

val json_error : string -> 'a

type lexer_state = {
  buf : Buffer.t;  (** Buffer used to accumulate substrings *)
  mutable lnum : int;  (** Current line number (counting from 1) *)
  mutable bol : int;
    (** Absolute position of the first character of the current line
        (counting from 0) *)
  mutable fname : string option;
    (** Name referencing the input file in error messages *)
}

module Lexer_state : sig
  type t = lexer_state = {
    buf : Buffer.t;
    mutable lnum : int;
    mutable bol : int;
    mutable fname : string option;
  }
end

val init_lexer :
  ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> unit -> lexer_state
(** Create a fresh lexer_state record. *)

(**/**)
(* begin undocumented section *)

exception End_of_array
exception End_of_object
exception End_of_tuple
exception End_of_input

(* end undocumented section *)
(**/**)

(** {2 JSON pretty-printing} *)

val pretty_print : ?std:bool -> Format.formatter -> t -> unit
(** Pretty-print into a {!Format.formatter}.
      See [to_string] for the role of the optional [std] argument.

      @since 1.3.1 *)

val pretty_to_string : ?std:bool -> t -> string
(** Pretty-print into a string.
      See [to_string] for the role of the optional [std] argument.
  *)

val pretty_to_channel : ?std:bool -> out_channel -> t -> unit
(** Pretty-print to a channel.
      See [to_string] for the role of the optional [std] argument.
  *)

module Util : sig
  (**
   This module provides combinators for extracting fields from JSON
   values. This approach is recommended for reading a few fields
   from data returned by public APIs. However for more complex applications
   we recommend {{:https://github.com/ahrefs/atd}Atdgen}.

   Here is some sample JSON data:
{v
{
  "id": "398eb027",
  "name": "John Doe",
  "pages": [
    {
      "id": 1,
      "title": "The Art of Flipping Coins",
      "url": "http://example.com/398eb027/1"
    },
    {
      "id": 2,
      "deleted": true
    },
    {
      "id": 3,
      "title": "Artichoke Salad",
      "url": "http://example.com/398eb027/3"
    },
    {
      "id": 4,
      "title": "Flying Bananas",
      "url": "http://example.com/398eb027/4"
    }
  ]
}
v}

   In order to extract the "id" field, assuming it is mandatory,
   we would use the following OCaml code that operates on single JSON
   nodes:
{v
open Json.Util
...
  let id = json |> member "id" |> to_string in
  ...
v}

   In order to extract all the "title" fields, we would write the following
   OCaml code that operates on lists of JSON nodes, skipping
   undefined nodes and nodes of unexpected type:
{v
open Json.Util

let extract_titles (json : t) : string list =
  [json]
    |> filter_member "pages"
    |> flatten
    |> filter_member "title"
    |> filter_string
v}
*)

  exception Type_error of string * t
  (** Raised when the JSON value is not of the correct type to support an
      operation, e.g. [member] on an [`Int]. The string message explains the
      mismatch. *)

  exception Undefined of string * t
  (** Raised when the equivalent JavaScript operation on the JSON value would
      return undefined. Currently this only happens when an array index is out
      of bounds. *)

  val keys : t -> string list
  (** Returns all the key names in the given JSON object *)

  val values : t -> t list
  (** Return all the value in the given JSON object *)

  val combine : t -> t -> t
  (** Combine two JSON Objects together *)

  val member : string -> t -> t
  (** [member k obj] returns the value associated with the key [k] in the JSON
      object [obj], or [`Null] if [k] is not present in [obj]. *)

  val index : int -> t -> t
  (** [index i arr] returns the value at index [i] in the JSON array [arr].
      Negative indices count from the end of the list (so -1 is the last
      element). *)

  val map : (t -> t) -> t -> t
  (** [map f arr] calls the function [f] on each element of the JSON array
      [arr], and returns a JSON array containing the results. *)

  val to_assoc : t -> (string * t) list
  (** Extract the items of a JSON object or raise [Type_error]. *)

  val to_option : (t -> 'a) -> t -> 'a option
  (** Return [None] if the JSON value is null or map the JSON value
      to [Some] value using the provided function. *)

  val to_bool : t -> bool
  (** Extract a boolean value or raise [Type_error]. *)

  val to_bool_option : t -> bool option
  (** Extract [Some] boolean value,
      return [None] if the value is null,
      or raise [Type_error] otherwise. *)

  val to_number : t -> float
  (** Extract a number or raise [Type_error]. *)

  val to_number_option : t -> float option
  (** Extract [Some] number,
      return [None] if the value is null,
      or raise [Type_error] otherwise. *)

  val to_float : t -> float
  (** Extract a float value or raise [Type_error].
      [to_number] is generally preferred as it also works with int literals. *)

  val to_float_option : t -> float option
  (** Extract [Some] float value,
      return [None] if the value is null,
      or raise [Type_error] otherwise.
      [to_number_option] is generally preferred as it also works
      with int literals. *)

  val to_int : t -> int
  (** Extract an int from a JSON int or raise [Type_error]. *)

  val to_int_option : t -> int option
  (** Extract [Some] int from a JSON int,
      return [None] if the value is null,
      or raise [Type_error] otherwise. *)

  val to_list : t -> t list
  (** Extract a list from JSON array or raise [Type_error]. *)

  val to_string : t -> string
  (** Extract a string from a JSON string or raise [Type_error]. *)

  val to_string_option : t -> string option
  (** Extract [Some] string from a JSON string,
      return [None] if the value is null,
      or raise [Type_error] otherwise. *)

  val convert_each : (t -> 'a) -> t -> 'a list
  (** The conversion functions above cannot be used with [map], because they do
      not return JSON values. This convenience function [convert_each to_f arr]
      is equivalent to [List.map to_f (to_list arr)]. *)

  (** {3 Exception-free filters} *)

  (**
   The following functions operate on lists of JSON nodes.
   None of them raises an exception when a certain kind of node is expected
   but no node or the wrong kind of node is found.
   Instead of raising an exception, nodes that are not as expected
   are simply ignored.
*)

  val filter_map : ('a -> 'b option) -> 'a list -> 'b list
  (** [filter_map f l] maps each element of the list [l] to an optional value
      using function [f] and unwraps the resulting values. *)

  val flatten : t list -> t list
  (** Expects JSON arrays and returns all their elements as a single
      list. [flatten l] is equivalent to [List.flatten (filter_list l)]. *)

  val filter_index : int -> t list -> t list
  (** Expects JSON arrays and returns all their elements existing at the given
      position. *)

  val filter_list : t list -> t list list
  (** Expects JSON arrays and unwraps them. *)

  val filter_member : string -> t list -> t list
  (** Expects JSON objects and returns all the fields of the given name
      (at most one field per object). *)

  val filter_assoc : t list -> (string * t) list list
  (** Expects JSON objects and unwraps them. *)

  val filter_bool : t list -> bool list
  (** Expects JSON booleans and unwraps them. *)

  val filter_int : t list -> int list
  (** Expects JSON integers ([`Int] nodes) and unwraps them. *)

  val filter_float : t list -> float list
  (** Expects JSON floats ([`Float] nodes) and unwraps them. *)

  val filter_number : t list -> float list
  (** Expects JSON numbers ([`Int] or [`Float]) and unwraps them.
      Ints are converted to floats. *)

  val filter_string : t list -> string list
  (** Expects JSON strings and unwraps them. *)
end

module Read : sig
  val prettify : ?std:bool -> string -> string
  (** Combined parser and pretty-printer.
      See [to_string] for the role of the optional [std] argument. *)

  val compact : ?std:bool -> string -> string
  (** Combined parser and printer.
      See [to_string] for the role of the optional [std] argument. *)

  (** {2 JSON readers} *)

  exception Finally of exn * exn
  (** Exception describing a failure in both finalizer and parsing. *)

  val from_string : ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> string -> t
  (** Read a JSON value from a string.
      @param buf use this buffer at will during parsing instead of creating
      a new one.
      @param fname data file name to be used in error messages. It does
      not have to be a real file.
      @param lnum number of the first line of input. Default is 1.
  *)

  val from_channel :
    ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> in_channel -> t
  (** Read a JSON value from a channel.
      See [from_string] for the meaning of the optional arguments. *)

  val from_file : ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> string -> t
  (** Read a JSON value from a file.
      See [from_string] for the meaning of the optional arguments. *)

  type lexer_state = Json_common.lexer_state
  (** This alias is provided for backward compatibility.
    *)

  val init_lexer :
    ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> unit -> lexer_state
  (** This alias is provided for backward compatibility. *)

  val from_lexbuf : lexer_state -> ?stream:bool -> Lexing.lexbuf -> t
  (** Read a JSON value from a lexbuf.
      A valid initial [lexer_state] can be created with [init_lexer].
      See [from_string] for the meaning of the optional arguments.

      @param stream indicates whether more data may follow. The default value
      is false and indicates that only JSON whitespace can be found between
      the end of the JSON value and the end of the input. *)

  type json_line = [ `Json of t | `Exn of exn ]
  (** The type of values resulting from a parsing attempt of a JSON value. *)

  val read_t : lexer_state -> Lexing.lexbuf -> t
  (** Read a JSON value from the given lexer_state and lexing buffer and return it.
    Provided as a reader function for atdgen.
*)

  (**/**)
  (* begin undocumented section *)

  val finish_string : lexer_state -> Lexing.lexbuf -> string
  val read_string : lexer_state -> Lexing.lexbuf -> string
  val read_ident : lexer_state -> Lexing.lexbuf -> string

  val map_string :
    lexer_state -> (string -> int -> int -> 'a) -> Lexing.lexbuf -> 'a
  (* equivalent to finish_string *)

  val map_ident :
    lexer_state -> (string -> int -> int -> 'a) -> Lexing.lexbuf -> 'a
  (* equivalent to read_ident *)

  type variant_kind = [ `Edgy_bracket | `Square_bracket | `Double_quote ]

  val start_any_variant : lexer_state -> Lexing.lexbuf -> variant_kind
  val finish_variant : lexer_state -> Lexing.lexbuf -> t option
  val finish_skip_variant : lexer_state -> Lexing.lexbuf -> unit
  val read_lt : lexer_state -> Lexing.lexbuf -> unit
  val read_gt : lexer_state -> Lexing.lexbuf -> unit
  val read_comma : lexer_state -> Lexing.lexbuf -> unit
  val finish_stringlit : lexer_state -> Lexing.lexbuf -> string
  val finish_skip_stringlit : lexer_state -> Lexing.lexbuf -> unit
  val finish_escaped_char : lexer_state -> Lexing.lexbuf -> unit
  val finish_comment : lexer_state -> Lexing.lexbuf -> unit
  val read_space : lexer_state -> Lexing.lexbuf -> unit
  val read_eof : Lexing.lexbuf -> bool
  val read_null : lexer_state -> Lexing.lexbuf -> unit
  val read_null_if_possible : lexer_state -> Lexing.lexbuf -> bool
  val read_bool : lexer_state -> Lexing.lexbuf -> bool
  val read_int : lexer_state -> Lexing.lexbuf -> int
  val read_int8 : lexer_state -> Lexing.lexbuf -> char
  val read_int32 : lexer_state -> Lexing.lexbuf -> int32
  val read_int64 : lexer_state -> Lexing.lexbuf -> int64
  val read_number : lexer_state -> Lexing.lexbuf -> float
  val skip_ident : lexer_state -> Lexing.lexbuf -> unit

  val read_sequence :
    ('a -> lexer_state -> Lexing.lexbuf -> 'a) ->
    'a ->
    lexer_state ->
    Lexing.lexbuf ->
    'a

  val read_list :
    (lexer_state -> Lexing.lexbuf -> 'a) ->
    lexer_state ->
    Lexing.lexbuf ->
    'a list

  val read_list_rev :
    (lexer_state -> Lexing.lexbuf -> 'a) ->
    lexer_state ->
    Lexing.lexbuf ->
    'a list

  val read_array_end : Lexing.lexbuf -> unit
  val read_array_sep : lexer_state -> Lexing.lexbuf -> unit

  val read_array :
    (lexer_state -> Lexing.lexbuf -> 'a) ->
    lexer_state ->
    Lexing.lexbuf ->
    'a array

  val read_tuple :
    (int -> 'a -> lexer_state -> Lexing.lexbuf -> 'a) ->
    'a ->
    lexer_state ->
    Lexing.lexbuf ->
    'a

  val start_any_tuple : lexer_state -> Lexing.lexbuf -> bool
  val read_lpar : lexer_state -> Lexing.lexbuf -> unit
  val read_rpar : lexer_state -> Lexing.lexbuf -> unit
  val read_tuple_end : Lexing.lexbuf -> unit
  val read_tuple_end2 : lexer_state -> bool -> Lexing.lexbuf -> unit
  val read_tuple_sep : lexer_state -> Lexing.lexbuf -> unit
  val read_tuple_sep2 : lexer_state -> bool -> Lexing.lexbuf -> unit
  val read_lbr : lexer_state -> Lexing.lexbuf -> unit
  val read_rbr : lexer_state -> Lexing.lexbuf -> unit

  val read_fields :
    ('acc -> string -> lexer_state -> Lexing.lexbuf -> 'acc) ->
    'acc ->
    lexer_state ->
    Lexing.lexbuf ->
    'acc

  val read_abstract_fields :
    (lexer_state -> Lexing.lexbuf -> 'key) ->
    ('acc -> 'key -> lexer_state -> Lexing.lexbuf -> 'acc) ->
    'acc ->
    lexer_state ->
    Lexing.lexbuf ->
    'acc

  val read_lcurl : lexer_state -> Lexing.lexbuf -> unit
  val read_object_end : Lexing.lexbuf -> unit
  val read_object_sep : lexer_state -> Lexing.lexbuf -> unit
  val read_colon : lexer_state -> Lexing.lexbuf -> unit
  val read_json : lexer_state -> Lexing.lexbuf -> t
  val skip_json : lexer_state -> Lexing.lexbuf -> unit
  val buffer_json : lexer_state -> Lexing.lexbuf -> unit
  val validate_json : 'path -> t -> 'error option
end

val pp : Format.formatter -> t -> unit
(** Pretty printer, useful for debugging *)

val show : t -> string
(** Convert value to string, useful for debugging *)

val equal : t -> t -> bool
(** [equal a b] is the monomorphic equality.
      Determines whether two JSON values are considered equal. In the case of
      JSON objects, the order of the keys does not matter, except for
      duplicate keys which will be considered equal as long as they are in the
      same input order.
    *)

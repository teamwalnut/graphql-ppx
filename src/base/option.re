exception Option_unwrap_error;

let map = f =>
  fun
  | None => None
  | Some(v) => Some(f(v));

let flat_map = f =>
  fun
  | None => None
  | Some(v) => f(v);

let unsafe_unwrap =
  fun
  | None => raise(Option_unwrap_error)
  | Some(v) => v;

let get_or_else = default => 
  fun
  | None => default
  | Some(v) => v;

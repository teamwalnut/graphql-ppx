exception Option_unwrap_error(string);

let map = f =>
  fun
  | None => None
  | Some(v) => Some(f(v));

let flat_map = f =>
  fun
  | None => None
  | Some(v) => f(v);

let unsafe_unwrap = (~reason) =>
  fun
  | None => raise(Option_unwrap_error(reason))
  | Some(v) => v;

let get_or_else = default =>
  fun
  | None => default
  | Some(v) => v;

let is_some =
  fun
  | Some(_) => true
  | None => false;

let is_none = x => !is_some(x);

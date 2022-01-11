exception Option_unwrap_error of string

let map f = function None -> None | Some v -> Some (f v)
let flat_map f = function None -> None | Some v -> f v

let unsafe_unwrap ~reason = function
  | None -> raise (Option_unwrap_error reason)
  | Some v -> v

let get_or_else default = function None -> default | Some v -> v
let is_some = function Some _ -> true | None -> false
let is_none x = not (is_some x)

let log_head = "[dirty_checker]"

exception File_not_found

let hash file =
  let () = Log.log ("[calc hash of] " ^ file) in
  Digest.file file

let read_hash src =
  let () = Log.log ("[read hash from] " ^ src) in
  match open_in_bin src with
  | exception _ -> None
  | hash_file -> (
    match Digest.input hash_file with
    | hash ->
      close_in hash_file;
      Some hash
    | exception End_of_file -> None)

let write_hash hash dest =
  let () = Log.log ("[write hash to] " ^ dest) in
  (match open_out_bin dest with
  | dest ->
    Digest.output dest hash;
    close_out dest
  | exception _ -> ());
  ()

let find_file file =
  if Sys.file_exists file then
    let () = Log.log ("[found] " ^ file) in
    Some file
  else
    let () = Log.log ("[not found] " ^ file) in
    None

type checker = {
  src : string;
  hash_path : string;
  dirty_callback : string -> unit;
}

exception Dirty_update_failure of string

let dirty_update hash checker =
  Log.log "[resource dirty!]";
  match checker.dirty_callback checker.src with
  | () ->
    write_hash hash checker.hash_path;
    Log.log "[update ok]"
  | exception Dirty_update_failure msg -> Log.error_log ("[update error] " ^ msg)

let check checker =
  match find_file checker.src with
  | None -> raise File_not_found
  | Some file -> (
    let new_hash = hash file in
    match read_hash checker.hash_path with
    | None -> dirty_update new_hash checker
    | Some old_hash -> (
      match Digest.compare old_hash new_hash with
      | 0 -> Log.log "[resource unchanged]"
      | _ -> dirty_update new_hash checker))

let log_head = "[dirty_checker]";

exception File_not_found;

let hash = file => {
  let () = Log.log("[calc hash of] " ++ file);
  Digest.file(file);
};

let read_hash = src => {
  let () = Log.log("[read hash from] " ++ src);
  switch (open_in_bin(src)) {
  | hash_file =>
    let hash = Digest.input(hash_file);
    close_in(hash_file);
    Some(hash);
  | exception _ => None
  };
};

let write_hash = (hash, dest) => {
  let () = Log.log("[write hash to] " ++ dest);
  switch (open_out_bin(dest)) {
  | dest =>
    Digest.output(dest, hash);
    close_out(dest);
  | exception _ => ()
  };
  ();
};

let find_file = file =>
  if (Sys.file_exists(file)) {
    let () = Log.log("[found] " ++ file);
    Some(file);
  } else {
    let () = Log.log("[not found] " ++ file);
    None;
  };

type checker = {
  src: string,
  hash_path: string,
  dirty_callback: string => unit,
};

exception Dirty_update_failure(string);

let dirty_update = (hash, checker) => {
  Log.log("[resource dirty!]");
  switch (checker.dirty_callback(checker.src)) {
  | () =>
    write_hash(hash, checker.hash_path);
    Log.log("[update ok]");
  | exception (Dirty_update_failure(msg)) =>
    Log.error_log("[update error] " ++ msg)
  };
};

let check = checker =>
  switch (find_file(checker.src)) {
  | None => raise(File_not_found)
  | Some(file) =>
    let new_hash = hash(file);
    switch (read_hash(checker.hash_path)) {
    | None => dirty_update(new_hash, checker) /* dirty */
    | Some(old_hash) =>
      switch (Digest.compare(old_hash, new_hash)) {
      | 0 => Log.log("[resource unchanged]")
      | _ => dirty_update(new_hash, checker)
      }
    };
  }; /* dirty */

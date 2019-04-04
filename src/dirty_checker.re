let log_head = "[dirty_checker]";

exception File_not_found;

let hash = file => {
  let () = Log.log("[calc hash of] " ++ file);
  Digest.file(file);
};

let read_hash = src => {
  let () = Log.log("[read hash from] " ++ src);
  let hash_file = open_in_bin(src);
  let hash = Digest.input(hash_file);
  close_in(hash_file);
  hash;
};

let write_hash = (hash, dest) => {
  let () = Log.log("[write hash to] " ++ dest);
  let dest = open_out_bin(dest);
  Digest.output(dest, hash);
  close_out(dest);
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
    Log.must_log("[update ok]");
  | exception (Dirty_update_failure(msg)) =>
    Log.must_log("[update error] " ++ msg)
  };
};

let check = checker =>
  switch (find_file(checker.src)) {
  | None => raise(File_not_found)
  | Some(file) =>
    let new_hash = hash(file);
    switch (find_file(checker.hash_path)) {
    | None => dirty_update(new_hash, checker) /* dirty */
    | Some(hash_file) =>
      let old_hash = read_hash(hash_file);
      switch (Digest.compare(old_hash, new_hash)) {
      | 0 => Log.log("[resource unchanged]")
      | _ => dirty_update(new_hash, checker)
      };
    };
  }; /* dirty */

open Helper;

module StringSet = Set.Make(String);

let map_with_concurrency = (concurrency, f) => {
  let queue = Lwt_pool.create(concurrency, () => Lwt.return());
  xs => xs |> List.map(x => Lwt_pool.use(queue, () => f(x))) |> Lwt.all;
};
let uniq_files = files =>
  files
  |> List.fold_left(
       ((env, files), (from, `To(to_))) => {
         let key = to_ |> String.lowercase_ascii;
         StringSet.find_opt(key, env) != None
           ? (env, files)
           : (env |> StringSet.add(key), [(from, `To(to_)), ...files]);
       },
       (StringSet.empty, []),
     )
  |> snd;

module Make =
       (
         Args: {
           let verbose: bool;
           let target: string;
           let install_folder: string;
         },
       ) => {
  open Args;
  let normalize_opam_to = (prefix, to_) => {
    // TODO: check if exists some exception
    let to_ =
      to_ |> Lib.starts_with(~pattern="../../")
        ? String.sub(to_, 3, String.length(to_) - 3) : to_;
    let dune_cross_pattern = Re.Pcre.regexp({|^\.\.\/(.*?)-sysroot\/|});
    let match = Re.exec_opt(dune_cross_pattern, to_);
    switch (match) {
    | Some(group) =>
      let found_target = Re.Group.get(group, 1);
      let to_ = to_ |> Re.replace_string(~by="", dune_cross_pattern);
      if (target != found_target || !Lib.starts_with(~pattern=prefix, to_)) {
        print_endline(prefix);
        failwith("thats weird: " ++ to_);
      };
      let from_ = String.length(prefix) + 1;
      String.sub(to_, from_, String.length(to_) - from_);
    | None => to_
    };
  };
  let get_prefix = (pkg_name, tag) => {
    let prefix =
      switch (tag) {
      | "lib" => ["lib", pkg_name]
      | "lib_root" => ["lib"]
      | "libexec" => ["lib", pkg_name]
      | "libexec_root" => ["lib"]
      | "bin" => ["bin"]
      | "sbin" => ["sbin"]
      | "toplevel" => ["lib", "toplevel"]
      | "share" => ["share", pkg_name]
      | "share_root" => ["share"]
      | "etc" => ["etc", pkg_name]
      | "doc" => ["doc", pkg_name]
      | "stublibs" => ["lib", "stublibs"]
      | "man" => ["man"]
      | _ => failwith("unknown tag: " ++ tag)
      };
    prefix |> String.concat("/");
  };
  let parse_tag = opam_line => {
    let rex = Re.Pcre.regexp({|(.*):|});
    let+some group = opam_line |> Re.exec_opt(rex);
    Re.Group.get(group, 1);
  };
  let parse_line = opam_line => {
    let+some group = opam_line |> Re.exec_opt(Re.Pcre.regexp({|"(.*?)"|}));
    let base_from = Re.Group.get(group, 1);

    let base_to = {
      let.apply () = Option.value(~default=".");
      let+some group =
        opam_line |> Re.exec_opt(Re.Pcre.regexp({|\{"(.*?)"\}|}));
      Re.Group.get(group, 1);
    };
    (`From(base_from), `To(base_to));
  };

  let parse = (pkg_name, opam_install) => {
    let lines =
      opam_install
      |> String.split_on_char('\n')
      |> List.map(String.trim)
      |> List.filter(line =>
           parse_tag(line)
           |> Option.is_some
           || parse_line(line)
           |> Option.is_some
         );
    lines
    |> List.fold_left(
         ((tag, files), line) => {
           switch (parse_tag(line), parse_line(line)) {
           | (Some(tag), _) => (tag, files)
           | (_, Some((`From(from), `To(base_to)))) =>
             let prefix = get_prefix(pkg_name, tag);
             let install_directory = Filename.concat(install_folder, prefix);
             let to_ =
               Filename.concat(
                 install_directory,
                 switch (base_to) {
                 | "" => Filename.basename(from)
                 | base_to => normalize_opam_to(prefix, base_to)
                 },
               );
             (tag, [(`From(from), `To(to_)), ...files]);
           | _ => failwith("unreachable")
           }
         },
         ("lib", []),
       )
    |> snd
    |> List.rev;
  };

  let readdir = dir => {
    let.await stat = Lwt_unix.stat(dir);
    switch (stat.Unix.st_kind) {
    | Unix.S_DIR => Lwt_unix.files_of_directory(dir) |> Lwt_stream.to_list
    | _ => await([])
    };
  };
  let readdir = dir => Lwt.catch(() => readdir(dir), _ => await([]));
  let main = () => {
    let pwd = Unix.getcwd();
    // TODO: use proper opam parsing
    let dirs = [pwd, Filename.concat(pwd, "default." ++ target)];
    let.await files =
      dirs
      |> List.map(dir => {
           let+await files = readdir(dir);
           files |> List.map(file => Filename.concat(dir, file));
         })
      |> Lwt.all
      |> Lwt.map(List.concat);
    let.await files =
      files
      |> List.filter(Lib.ends_with(~pattern=".install"))
      |> List.sort(String.compare)
      |> List.map(file => {
           if (verbose) {
             Printf.printf("parsing manifest: %s\n%!", file);
           };
           let name = file |> Filename.basename |> Filename.remove_extension;
           let+await data = Lib.read_file(file);
           let pkg_name =
             Lib.ends_with(~pattern="-" ++ target, name)
               ? String.sub(
                   name,
                   0,
                   String.length(name) - String.length(target) - 1,
                 )
               : name;
           parse(pkg_name, data);
         })
      |> Lwt.all
      |> Lwt.map(manifests => manifests |> List.concat |> uniq_files);

    if (List.length(files) == 0) {
      exit(0);
    };

    let folders =
      files
      |> List.map(((_, `To(to_))) => Filename.dirname(to_))
      |> List.sort_uniq(String.compare);
    let.await () = Lib.mkdirp(folders |> String.concat(" "));

    // TODO: needed because limit of file descriptors
    let.await _ =
      files
      |> map_with_concurrency(
           32,
           ((`From(from), `To(to_))) => {
             let optional = from |> Lib.starts_with(~pattern="?");
             let from = {
               let from =
                 optional
                   ? String.sub(from, 1, String.length(from) - 1) : from;
               Filename.is_relative(from)
                 ? Filename.concat(pwd, from) : from;
             };
             let.await exists = Lwt_unix.file_exists(from);
             switch (optional, exists) {
             | (true, false) => await()
             // TODO: should I ensure the file exists even when not optional?
             | _ =>
               let+await _ = Lib.exec("ln -sfn " ++ from ++ " " ++ to_);
               ();
             };
           },
         );
    await();
  };
};

module Installer =
  Make({
    let install_folder = Sys.getenv("cur__install");
    let target = Sys.argv[1];
    let verbose = Sys.getenv_opt("VERBOSE") != None;
  });
Installer.main() |> Lwt_main.run;

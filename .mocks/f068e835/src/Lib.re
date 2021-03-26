open Helper;

// TODO: use Path.t
let read_file = path => {
  // TODO: do I need to close it?
  let.await file = Lwt_io.open_file(~mode=Input, path);
  Lwt_io.read(file);
};

// TODO: sh -c will fail when cmd length > MAX_ARG_STRLEN
let exec = cmd => cmd |> Lwt_process.shell |> Lwt_process.pread;

let mkdirp = folder => exec("mkdir -p " ++ folder) |> Lwt.map(_ => ());

let starts_with = (~pattern, name) => {
  let length = String.length(pattern);
  String.length(name) >= length && String.sub(name, 0, length) == pattern;
};

let rev_string = s =>
  s |> String.to_seq |> List.of_seq |> List.rev |> List.to_seq |> String.of_seq;
let ends_with = (~pattern, name) =>
  starts_with(~pattern=rev_string(pattern), rev_string(name));

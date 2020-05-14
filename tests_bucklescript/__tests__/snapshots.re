open Jest;
open Expect;

type options = {cwd: string};
type buffer;

[@bs.module "path"] external resolve: (string, string) => string = "resolve";

[@bs.module "child_process"]
external execSync: (string, options) => buffer = "execSync";

type errorObj = {message: string};
type spawnResult = {
  stdout: Js.Nullable.t(buffer),
  stderr: Js.Nullable.t(buffer),
  error: Js.Nullable.t(errorObj),
};
[@bs.module "child_process"]
external spawnSync: (string, array(string), options) => spawnResult =
  "spawnSync";

[@bs.module "child_process"]
external exec:
  (string, options, (Js.nullable(Js.t({..})), string, string) => unit) =>
  unit =
  "exec";

[@bs.module "fs"]
external readdirSync: string => array(string) = "readdirSync";

[@bs.module "fs"]
external writeFileSync: (string, string) => unit = "writeFileSync";

[@bs.module "os"] external platform: unit => string = "platform";

[@bs.val] external dirname: string = "__dirname";
[@bs.send] external toString: buffer => string = "toString";

let win = platform() == "win32";

let execSyncWithErr = (cmd, args, opts) => {
  let result = spawnSync(cmd, args, opts);
  let result =
    switch (
      Js.Nullable.toOption(result.stdout),
      Js.Nullable.toOption(result.stderr),
      Js.Nullable.toOption(result.error),
    ) {
    | (Some(stdout), Some(stderr), _) => (
        toString(stdout),
        toString(stderr),
      )
    | (_, _, Some(error)) => ("", error.message)
    | _ => ("", "")
    };
  result;
};

let removeKnownError = error => {
  Js.String.trim(
    Js.String.replaceByRe(
      Js.Re.fromString(
        "/node: /usr/lib/libstdc++.so.6: no version information available (required by node)/g",
      ),
      "",
      error,
    ),
  );
};

let refmt =
  execSync(
    "esy build echo \"#{@opam/reason.bin / }refmt\"",
    {cwd: resolve(dirname, "../..")},
  )
  |> toString
  |> Js.String.trim;

let run_ppx = (path, opts, testType) => {
  let result =
    execSync(
      (win ? "type " : "cat ")
      ++ path
      ++ " | "
      ++ refmt
      ++ " --parse re --print binary | ../_build/default/src/bucklescript_bin/bin.exe -schema ../graphql_schema.json "
      ++ opts
      ++ (win ? " - -o -" : " /dev/stdin /dev/stdout")
      ++ " | "
      ++ refmt
      ++ " --parse binary --interface false",
      {cwd: resolve(dirname, "..")},
    )
    |> toString;
  writeFileSync("static_snapshots/" ++ testType ++ "/" ++ path, result);
  result;
};

let tests =
  readdirSync("operations")->Belt.Array.keep(Js.String.endsWith(".re"));

describe("Legacy", () =>
  tests
  |> Array.iter(t => {
       test(t, () =>
         expect(run_ppx("operations/" ++ t, "-legacy", "legacy"))
         |> toMatchSnapshot
       )
     })
);

describe("Objects", () =>
  tests
  |> Array.iter(t => {
       test(t, () =>
         expect(run_ppx("operations/" ++ t, "-objects", "objects"))
         |> toMatchSnapshot
       )
     })
);

describe("Records", () =>
  tests
  |> Array.iter(t => {
       test(t, () =>
         expect(run_ppx("operations/" ++ t, "", "records"))
         |> toMatchSnapshot
       )
     })
);

describe("Apollo", () =>
  tests
  |> Array.iter(t => {
       test(t, () =>
         expect(run_ppx("operations/" ++ t, "-apollo-mode", "apollo-mode"))
         |> toMatchSnapshot
       )
     })
);

let get_bsb_error = (~ppxOptions, ~fileName, ~pathIn: string) => {
  let (_output, err) =
    execSyncWithErr(
      "./node_modules/.bin/bsc",
      [|
        "-c",
        "-w",
        "-30",
        "-ppx",
        "../_build/default/src/bucklescript_bin/bin.exe " ++ ppxOptions,
        pathIn ++ "/" ++ fileName,
      |],
      {cwd: resolve(dirname, "..")},
    );
  err;
};

let get_bsb_output = (~ppxOptions, ~fileName, ~pathIn: string) =>
  execSyncWithErr(
    "./node_modules/.bin/bsc",
    [|
      "-c",
      "-w",
      "-30",
      "-ppx",
      "../_build/default/src/bucklescript_bin/bin.exe " ++ ppxOptions,
      pathIn ++ "/" ++ fileName,
    |],
    {cwd: resolve(dirname, "..")},
  );

let get_bsb_error_with_static_snapshot =
    (~ppxOptions, ~fileName, ~pathIn, ~pathOut) => {
  let stderr = get_bsb_error(~ppxOptions, ~fileName, ~pathIn);
  let result = {
    let lines = stderr |> Js.String.split("\n");
    let stderr =
      lines
      |> Js.Array.reduce(
           (p, ln) => {
             p !== ""
               ? p ++ "\n" ++ ln
               : ln
                 |> Js.String.includes("operations/")
                 || ln
                 |> Js.String.includes("found a bug for you")
                   ? ln : p
           },
           "",
         );
    let cutPosition =
      stderr |> Js.String.indexOf("Error while running external preprocessor");
    let to_ = cutPosition > (-1) ? cutPosition : stderr |> Js.String.length;
    stderr |> Js.String.substring(~from=0, ~to_) |> Js.String.trim;
  };

  let newFileName =
    (
      fileName
      |> Js.String.substring(~from=0, ~to_=(fileName |> Js.String.length) - 3)
    )
    ++ ".txt";
  writeFileSync({j|static_snapshots/$pathOut/$newFileName|j}, result ++ "\n");
  result;
};

let get_bsb_output_with_static_snapshot =
    (~ppxOptions, ~fileName, ~pathIn, ~pathOut) => {
  let (output, err) = get_bsb_output(~ppxOptions, ~fileName, ~pathIn);
  let newFileName =
    (
      fileName
      |> Js.String.substring(~from=0, ~to_=(fileName |> Js.String.length) - 3)
    )
    ++ ".js";
  writeFileSync({j|static_snapshots/$pathOut/$newFileName|j}, output);
  (output, err);
};

describe("Compilation (Objects)", () =>
  tests
  |> Array.iter(t => {
       let (output, err) =
         get_bsb_output_with_static_snapshot(
           ~ppxOptions="-objects",
           ~fileName=t,
           ~pathIn="operations",
           ~pathOut="objects/operations",
         );
       test(t, () =>
         expect(output) |> toMatchSnapshot
       );
       test(t, () =>
         expect(removeKnownError(err)) |> toBe("")
       );
     })
);

describe("Compilation (Records)", () =>
  tests
  |> Array.iter(t => {
       let (output, err) =
         get_bsb_output_with_static_snapshot(
           ~ppxOptions="",
           ~fileName=t,
           ~pathIn="operations",
           ~pathOut="records/operations",
         );
       test(t, () =>
         expect(output) |> toMatchSnapshot
       );
       test(t, () => {
         expect(removeKnownError(err)) |> toBe("")
       });
     })
);

let tests =
  readdirSync("operations/errors")
  ->Belt.Array.keep(Js.String.endsWith(".re"));

describe("Errors (Records)", () =>
  tests
  |> Array.iter(t => {
       let result =
         get_bsb_error_with_static_snapshot(
           ~ppxOptions="",
           ~fileName=t,
           ~pathIn="operations/errors",
           ~pathOut="records/errors",
         );
       test(t, () =>
         expect(result) |> toMatchSnapshot
       );
     })
);

describe("Errors (Objects)", () =>
  tests
  |> Array.iter(t => {
       let result =
         get_bsb_error_with_static_snapshot(
           ~ppxOptions="-objects",
           ~fileName=t,
           ~pathIn="operations/errors",
           ~pathOut="objects/errors",
         );
       test(t, () =>
         expect(result) |> toMatchSnapshot
       );
     })
);

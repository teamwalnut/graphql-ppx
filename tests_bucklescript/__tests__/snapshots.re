open Jest;
open Expect;

type options = {cwd: string};
type buffer;

[@bs.module "path"] external resolve: (string, string) => string = "resolve";

[@bs.module "child_process"]
external execSync: (string, options) => buffer = "execSync";
[@bs.module "fs"]
external readdirSync: string => array(string) = "readdirSync";

[@bs.module "fs"]
external writeFileSync: (string, string) => unit = "writeFileSync";

[@bs.module "os"] external platform: unit => string = "platform";

[@bs.val] external dirname: string = "__dirname";
[@bs.send] external toString: buffer => string = "toString";

let win = platform() == "win32";
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

let tests =
  readdirSync("operations/errors")
  ->Belt.Array.keep(Js.String.endsWith(".re"));

let run_bsc_with_ppx = (fileName, pathIn, pathOut) => {
  let result =
    try(
      execSync(
        {j|./node_modules/.bin/bsc -ppx ../_build/default/src/bucklescript_bin/bin.exe $pathIn/$fileName|j},
        {cwd: resolve(dirname, "..")},
      )
      |> toString
    ) {
    | error =>
      let stderr = Js.Exn.asJsExn(error)->Obj.magic##stderr##toString();
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
        stderr
        |> Js.String.indexOf("Error while running external preprocessor");
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

describe("Errors", () =>
  tests
  |> Array.iter(t => {
       test(t, () =>
         expect(
           run_bsc_with_ppx(t, "operations/errors", "errors/operations"),
         )
         |> toMatchSnapshot
       )
     })
);

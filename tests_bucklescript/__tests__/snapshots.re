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

describe("Objects (legacy)", () =>
  tests
  |> Array.iter(t => {
       test(t, () =>
         expect(run_ppx("operations/" ++ t, "", "objects"))
         |> toMatchSnapshot
       )
     })
);

describe("Records", () =>
  tests
  |> Array.iter(t => {
       test(t, () =>
         expect(run_ppx("operations/" ++ t, "-records", "records"))
         |> toMatchSnapshot
       )
     })
);

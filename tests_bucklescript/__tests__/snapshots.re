open Jest;
open Expect;

type options = {cwd: string};
type buffer;

[@bs.module "path"] external resolve: (string, string) => string = "resolve";

[@bs.module "child_process"]
external execSync: (string, options) => buffer = "execSync";
[@bs.module "fs"]
external readdirSync: string => array(string) = "readdirSync";
[@bs.module "os"] external platform: unit => string = "platform";

[@bs.val] external dirname: string = "__dirname";
[@bs.send] external toString: buffer => string = "toString";

let win = platform() == "win32";
let refmt = execSync("esy install -P wait_pr_to_be_merged", {cwd: resolve(dirname, "../..")},);
let refmt =
  execSync(
    "esy build -P wait_pr_to_be_merged echo \"#{@opam/reason.bin / }refmt\"",
    {cwd: resolve(dirname, "../..")},
  )
  |> toString
  |> Js.String.trim;

let ppx = {|"../_build/default/src/bucklescript_bin/bin.exe"|};

let rm = win ? "del" : "rm";

let run_ppx = (path, opts) => {
  let _:buffer = execSync(refmt ++ " --parse re --print binary " ++ path ++ " > " ++ path ++ ".ml", {cwd: resolve(dirname, "..")});
  let _:buffer = execSync(ppx ++ " -schema ../graphql_schema.json --dump-ast " ++ opts ++ " " ++ path ++ ".ml " ++ path ++ ".pp.ml", {cwd: resolve(dirname, "..")});
  let ret = execSync(refmt ++ " --parse binary --print re " ++ path ++ ".pp.ml", {cwd: resolve(dirname, "..")})
  |> toString;
  // here we should clean file.
  ret;
};

let tests =
  readdirSync("operations")->Belt.Array.keep(Js.String.endsWith(".re"));

describe("Objects (legacy)", () => {
  tests
  |> Array.iter(t => {
       test(t, () =>
         expect(run_ppx("operations/" ++ t, "")) |> toMatchSnapshot
       )
     })
});

describe("Records", () => {
  tests
  |> Array.iter(t => {
       test(t, () =>
         expect(run_ppx("operations/" ++ t, "-records")) |> toMatchSnapshot
       )
     })
});

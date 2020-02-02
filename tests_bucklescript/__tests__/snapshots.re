open Jest;
open Expect;
open! Expect.Operators;

type options = {cwd: string};
type buffer;

[@bs.module "child_process"]
external execSync: (string, options) => buffer = "execSync";
[@bs.module "fs"]
external readdirSync: string => array(string) = "readdirSync";

[@bs.val] external dirname: string = "__dirname";
[@bs.send] external toString: buffer => string = "toString";

let refmt =
  execSync("esy x /usr/bin/which refmt", {cwd: ".."})
  |> toString
  |> Js.String.trim;

let run_ppx = (path, opts) => {
  execSync(
    "cat "
    ++ path
    ++ " | "
    ++ refmt
    ++ " --parse re --print binary | ../_build/default/src/bucklescript_bin/bin.exe -schema ../graphql_schema.json "
    ++ opts
    ++ " /dev/stdin /dev/stdout |  "
    ++ refmt
    ++ " --parse binary --print re --interface false",
    {cwd: "."},
  )
  |> toString;
};

let tests = readdirSync("operations");

describe("Objects (legacy)", () => {
  tests
  |> Array.map(t => {
       test(t, () =>
         expect(run_ppx("operations/" ++ t, "")) |> toMatchSnapshot
       )
     })
  |> ignore
});

describe("Records", () => {
  tests
  |> Array.map(t => {
       test(t, () =>
         expect(run_ppx("operations/" ++ t, "-records")) |> toMatchSnapshot
       )
     })
  |> ignore
});

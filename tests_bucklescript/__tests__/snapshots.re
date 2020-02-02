open Jest;

let dirname: option(string) = [%bs.node __dirname];

type options = {cwd: string};
type buffer;
[@bs.module "child_process"]
external execSync: (string, options) => buffer = "execSync";
[@bs.val] external dirname: string = "__dirname";
[@bs.send] external toString: buffer => string = "toString";

let refmt =
  execSync("esy x /usr/bin/which refmt", {cwd: ".."})
  |> toString
  |> Js.String.trim;

describe("objects", () => {
  open Expect;
  open! Expect.Operators;

  let run_ppx = path => {
    // let refmt = "~/.esy/3____________________________________________________________________/i/esy_ocaml__s__reason-3.5.2-7228114e/bin/refmt";
    execSync(
      "cat "
      ++ path
      ++ " | "
      ++ refmt
      ++ " --parse re --print binary | ../_build/default/src/bucklescript_bin/bin.exe -schema ../graphql_schema.json /dev/stdin /dev/stdout |  "
      ++ refmt
      ++ " --parse binary --print re --interface false",
      {cwd: "."},
    )
    |> toString;
  };

  test("argNamedQuery", () =>
    expect(run_ppx("object-tests/argNamedQuery.re")) |> toMatchSnapshot
  );
});

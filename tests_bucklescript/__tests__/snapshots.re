open Jest;
open Expect;

type options = {cwd: string};
type buffer;

[@bs.module "path"] external resolve: (string, string) => string = "resolve";

[@bs.module "child_process"]
external execSync: (string, options) => buffer = "execSync";
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
let _:buffer = execSync("esy install", {cwd: resolve(dirname, "../..")},);
let refmt =
  execSync(
    "esy build echo \"#{@opam/reason.bin / }refmt\"",
    {cwd: resolve(dirname, "../..")},
  )
  |> toString
  |> Js.String.trim;
let refmt = "\"" ++ refmt ++ "\"";

let ppx = Filename.( "bin.exe" |> concat("bucklescript_bin") |> concat("src") |> concat("default") |> concat("_build") |> concat(".."));

let rm = win ? "del" : "rm";

let run_ppx = (path, opts, testType) => {
  let _:buffer = execSync(refmt ++ " --parse re --print binary " ++ path ++ " > " ++ path ++ ".ml", {cwd: resolve(dirname, "..")});
  let _:buffer = execSync(ppx ++ " -schema ../graphql_schema.json --dump-ast " ++ opts ++ " " ++ path ++ ".ml " ++ path ++ ".pp.ml", {cwd: resolve(dirname, "..")});
  let ret = execSync(refmt ++ " --parse binary --print re " ++ path ++ ".pp.ml", {cwd: resolve(dirname, "..")})
  |> toString;
  writeFileSync("static_snapshots/" ++ testType ++ "/" ++ path, ret);
  // here we should clean file.
  ret;
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

let bsc = Filename.("bsc" |> concat(".bin") |> concat("node_modules") |> concat("."));

let tests =
  readdirSync("operations/errors")
  ->Belt.Array.keep(Js.String.endsWith(".re"));

let run_bsc_with_ppx = (fileName, pathIn, pathOut) => {
  Js.Promise.make((~resolve as resolvePromise, ~reject as _) => {
    exec(
      bsc ++ " -ppx " ++ ppx ++ {j| $pathIn/$fileName|j},
      {cwd: resolve(dirname, "..")},
      (_error, _stdout, stderr) => {
        let result = {
          let lines = stderr |> Js.String.replaceByRe([%re "/\\r\\n/g"], "\n") |> Js.String.split("\n");
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
          let to_ =
            cutPosition > (-1) ? cutPosition : stderr |> Js.String.length;
          stderr |> Js.String.substring(~from=0, ~to_) |> Js.String.trim;
        };

        let newFileName =
          (
            fileName
            |> Js.String.substring(
                 ~from=0,
                 ~to_=(fileName |> Js.String.length) - 3,
               )
          )
          ++ ".txt";
        let toBase64 = x => [%raw {|Buffer.from(x).toString("base64")|}];
        Js.log(toBase64(result));
        writeFileSync(
          {j|static_snapshots/$pathOut/$newFileName|j},
          result ++ "\n",
        );
        resolvePromise(. result);
      },
    )
  });
};

describe("Errors", () =>
  tests
  |> Array.iter(t => {
       testPromise(t, () =>
         run_bsc_with_ppx(t, "operations/errors", "errors/operations")
         |> Js.Promise.then_(result =>
              Js.Promise.resolve(expect(result) |> toMatchSnapshot)
            )
       )
     })
);

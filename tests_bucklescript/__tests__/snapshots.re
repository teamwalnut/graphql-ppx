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

let utils =
  readdirSync("utils")->Belt.Array.keep(Js.String.endsWith(".re"));

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

let get_bsb_error = (~fileName, ~pathIn: string) =>
  Js.Promise.make((~resolve as resolvePromise, ~reject as _) =>
    exec(
      {j|./node_modules/.bin/bsc -w -A -warn-error -A -ppx "../_build/default/src/bucklescript_bin/bin.exe" -bs-suffix -I ./utils $pathIn/$fileName|j},
      {cwd: resolve(dirname, "..")},
      (_error, _stdout, stderr) => {
      resolvePromise(. stderr)
    })
  );

let write_compilation_error_snapshot_to_disk = (fileName, pathIn, pathOut) =>
  get_bsb_error(~fileName, ~pathIn)
  |> Js.Promise.then_(stderr => {
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
       writeFileSync(
         {j|static_snapshots/$pathOut/$newFileName|j},
         result ++ "\n",
       );
       result |> Js.Promise.resolve;
     });

let runCompilationTests = () =>
  tests
  |> Array.iter(t =>
       testPromise(t, () =>
         get_bsb_error(~pathIn="operations", ~fileName=t)
         |> Js.Promise.then_(error =>
              expect(error) |> toEqual("") |> Js.Promise.resolve
            )
       )
     );

describe("Records compilation", () => {
  // We need to generate the compiler artifacts for all of the `util` files in order for manually building the `operations` files to work
  beforeAllPromise(() =>
    utils
    |> Array.map(t => get_bsb_error(~pathIn="utils", ~fileName=t))
    |> Js.Promise.all
  );
  runCompilationTests();
});

let tests =
  readdirSync("operations/errors")
  ->Belt.Array.keep(Js.String.endsWith(".re"));

describe("Errors", () =>
  tests
  |> Array.iter(t => {
       testPromise(t, () =>
         write_compilation_error_snapshot_to_disk(
           t,
           "operations/errors",
           "errors/operations",
         )
         |> Js.Promise.then_(result =>
              Js.Promise.resolve(expect(result) |> toMatchSnapshot)
            )
       )
     })
);

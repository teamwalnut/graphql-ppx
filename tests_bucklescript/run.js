const exec = require("child_process").exec;
const fs = require("fs");

function command(cmd) {
  console.log(cmd);
  return new Promise((resolve, reject) => {
    const process = exec(cmd, (error, stdout, stderr) => {
      if (error) reject(error);
      resolve(stdout ? stdout : stderr);
    });
    process.stdout.on("data", function(data) {
      console.log(data.toString());
    });
    process.stderr.on("data", function(data) {
      console.log(data.toString());
    });
  });
}

async function cleanup() {
  await command("rm -rf __tests__");
}

function writeConfig(flags = []) {
  fs.writeFileSync(
    "bsconfig.json",
    JSON.stringify({
      name: "tests_bucklescript",
      sources: ["__tests__"],
      "ppx-flags": [
        [
          "../_build/default/src/bucklescript_bin/bin.exe",
          ...flags,
          "-schema ../graphql_schema.json"
        ]
      ],
      "bs-dependencies": ["@glennsl/bs-jest"],
      refmt: 3,
      "bsc-flags": ["-bs-super-errors"],
      warnings: {
        number: "+A-48-30-42"
        // error: "+A-3-32-44"
      }
    })
  );
}

async function test(folder) {
  // object tests currently don't work yet

  // apollo mode
  // writeConfig(["-apollo-mode"]);
  // await command(`cp -r ./apollo-mode/ ./__tests__`);
  // await command("npm run test");

  // objects
  // writeConfig([]);
  // await command(`cp -r ./object-tests/ ./__tests__`);
  // await command("npm run test");

  // records
  writeConfig(["-apollo-mode", "-lean-parse"]);
  await command(`cp -r ./record-tests/ ./__tests__`);
  await command("npm run test");
  await cleanup();
}

async function run() {
  try {
    await test();
  } catch (error) {
    throw error;
  }
}

run();

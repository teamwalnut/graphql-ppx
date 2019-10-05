const exec = require("child_process").exec;

function command(cmd) {
  console.log(cmd);
  return new Promise((resolve, reject) => {
    const process = exec(cmd, (error, stdout, stderr) => {
      if (error) console.warn(error);
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
  await command("rm -f package.json");
  await command("rm -f bsconfig.json");
  await command("rm -f package-lock.json");
  await command("rm -rf node_modules");
}

async function test(folder) {
  await command(`cp ./${folder}/* .`);
  await command("npm install");
  await command("npm run test");
  // await cleanup();
}

async function run() {
  const [, , command] = process.argv;
  try {
    switch (command) {
      case "bsb5":
        await test("bsb5");
        break;
      case "bsb6":
        await test("bsb6");
        break;

      default:
        console.log(
          `Unknown comamnd: ${command}. Supported commands: bsb5, bsb6`
        );
        break;
    }
  } catch (error) {
    throw error;
  }
}

run();

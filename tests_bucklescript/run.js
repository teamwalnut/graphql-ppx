const exec = require("child_process").exec;

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
  await command("rm -f package.json");
  await command("rm -f bsconfig.json");
  await command("rm -f package-lock.json");
  await command("rm -rf node_modules");
}

async function test(folder, cleanup) {
  await command(`cp ./${folder}/* .`);
  await command("npm install");
  await command("npm run test");
  // await cleanup();
}

async function run() {
  const [, , folder, cleanup] = process.argv;
  try {
    await test(folder, toBool(cleanup, true));
  } catch (error) {
    throw error;
  }
}

function toBool(value, def) {
  if (value == "false") return false;
  if (value == "true") return true;
  return def;
}

run();

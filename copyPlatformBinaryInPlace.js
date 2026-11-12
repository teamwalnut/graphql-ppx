#!/usr/bin/env node

var fs = require("fs");
var path = require("path");

var arch = process.arch;
var platform = process.platform;

if (arch === "ia32") {
  arch = "x86";
}

if (platform === "win32") {
  platform = "win";
}

var platformBinary = path.join(
  __dirname,
  "bin",
  "graphql-ppx-" + platform + "-" + arch + ".exe"
);
var destBinary = path.join(__dirname, "ppx");

copyBinary(platformBinary, destBinary);

function copyBinary(filename, destFilename) {
  var supported = fs.existsSync(filename);

  if (!supported && !process.env.GRAPHQL_CI) {
    console.error("graphql-ppx does not support this platform :(");
    console.error("");
    console.error(
      "graphql-ppx comes prepacked as built binaries to avoid large"
    );
    console.error("dependencies at build-time.");
    console.error("");
    console.error("If you want graphql-ppx to support this platform natively,");
    console.error(
      "please open an issue at our repository, linked above. Please"
    );
    console.error("specify that you are on the " + platform + " platform,");
    console.error("on the " + arch + " architecture.");
    process.exit(0);
  }

  if (process.env.GRAPHQL_CI) {
    console.log(
      "graphql-ppx: CI has been set, skipping moving binary in place"
    );
    process.exit(0);
  }

  if (fs.existsSync(destFilename)) {
    fs.rmSync(destFilename);
  }
  fs.existsSync(destFilename);
  copyFileSync(filename, destFilename);
  fs.chmodSync(destFilename, 0x0755);

  // I am not sure why we need this but let's keep it for backwards compatibility
  // perhaps it's necessary for windows?
  // if it's not needed we can remove it later
  var destFilenameExe = destFilename + ".exe";
  if (fs.existsSync(destFilenameExe)) {
    fs.rmSync(destFilenameExe);
  }
  copyFileSync(filename, destFilenameExe);
  fs.chmodSync(destFilenameExe, 0x0755);
}

function copyFileSync(source, dest) {
  if (typeof fs.copyFileSync === "function") {
    fs.copyFileSync(source, dest);
  } else {
    fs.writeFileSync(dest, fs.readFileSync(source));
  }
}

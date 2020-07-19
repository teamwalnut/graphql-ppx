/* TestFramework.re */
include Rely.Make({
  let config =
    Rely.TestFrameworkConfig.initialize({
      snapshotDir: "tests_bucklescript/__snapshots__",
      projectDir: "tests_bucklescript",
    });
});

include
  (Rely.Make)(struct
                let config =
                  Rely.TestFrameworkConfig.initialize
                    {
                      snapshotDir = (("tests_bucklescript/__snapshots__")
                        [@reason.raw_literal
                          "tests_bucklescript/__snapshots__"]);
                      projectDir = (("tests_bucklescript")
                        )
                    }
              end)

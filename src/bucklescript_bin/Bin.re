open Migrate_parsetree;
open Graphql_ppx_bucklescript;

let _ = Driver.run_as_ppx_rewriter();

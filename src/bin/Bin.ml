Graphql_ppx_base.Ppx_config.update_config (fun current ->
  { current with native = false })

let () = Ppxlib.Driver.run_as_ppx_rewriter ()

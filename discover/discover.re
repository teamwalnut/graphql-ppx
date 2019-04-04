module C = Configurator.V1;

let () =
  C.main(~name="graphql_ppx", c => {
    let system = C.ocaml_config_var_exn(c, "system");
    let flags =
      if (system == "linux") {
        ["-ccopt", "-static"];
      } else {
        [];
      };
    C.Flags.write_sexp("dune.flags", flags);
  });

let log = msg =>
  if (Ppx_config.verbose_logging()) {
    print_endline(msg);
  };

let must_log = print_endline;

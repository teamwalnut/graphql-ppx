let log msg = if Ppx_config.verbose_logging () then print_endline msg
let error_log = prerr_endline

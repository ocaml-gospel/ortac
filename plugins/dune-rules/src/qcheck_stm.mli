type config = {
  interface_file : string;
  config_file : string option;
  ocaml_output : string option;
  library : string option;
  package_name : string option;
  dune_output : string option;
  fork_timeout : int option;
}

val gen_dune_rules : config Fmt.t

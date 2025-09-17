type config = {
  interface_file : string;
  config_file : string option;
  ocaml_output : string option;
  library : string option;
  package_name : string option;
  dune_output : string option;
  module_prefix : string option;
  submodule : string option;
  domain : bool;
  count : int;
  fork_timeout : int option;
  gen_alias : string option;
  run_alias : string option;
}

val gen_dune_rules : config Fmt.t

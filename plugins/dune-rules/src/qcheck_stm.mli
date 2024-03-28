type config = {
  interface_file : string;
  config_file : string;
  ocaml_output : string;
  package_name : string option;
  dune_output : string option;
}

val gen_dune_rules : config Fmt.t

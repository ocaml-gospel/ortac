type config = {
  interface_file : string;
  package_name : string option;
  ocaml_output : string option;
  dune_output : string option;
}

val gen_dune_rules : config Fmt.t

type config = {
  interface_file : string;
  init_function : string;
  sut_type : string;
  ocaml_output : string;
  include_ : string option;
  package_name : string option;
  dune_output : string option;
}

val gen_dune_rules : config Fmt.t

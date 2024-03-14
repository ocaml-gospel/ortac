val term_printer : string -> Ppxlib.Location.t -> Gospel.Tterm.term -> string
(** [term_printer text global_loc term] fetch the initial text representation of
    [term] provided that [text] is the specification's text and [global_loc] the
    specification's location. Fall back on the Gospel term pretty printer if
    something goes wrong when extracting the substring. *)

val module_name_of_path : string -> string
(** [module_name_of_path p] turn the path to an OCaml file [p] into the
    corresponding OCaml module identifier *)

val type_check :
  string list ->
  string ->
  Gospel.Uast.s_signature_item list ->
  Gospel.Tmodule.namespace list * Gospel.Tast.signature
(** [type_check load_path name sigs] call the Gospel typechecker on the file
    [name] *)

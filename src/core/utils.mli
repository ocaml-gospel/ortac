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

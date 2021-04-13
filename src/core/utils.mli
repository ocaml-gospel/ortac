val module_name_of_path : string -> string

val type_check :
  string list ->
  string ->
  Gospel.Uast.s_signature_item list ->
  Gospel.Tast.signature

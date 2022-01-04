include Ortac_runtime__Ortac_runtime_intf.S

val print_record : string -> (string * PPrint.document) list -> PPrint.document

val print_variant :
  string -> string -> int -> PPrint.document list -> PPrint.document

val print_tuple : PPrint.document list -> PPrint.document
val constructible_int : (int, int) Monolith.spec
val int : (int, int) Monolith.spec
val positive_int : (int, int) Monolith.spec
val array : ('r, 'c) Monolith.spec -> ('r array, 'c array) Monolith.spec

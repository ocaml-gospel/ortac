val print_record :
  string -> (string * PPrintEngine.document) list -> PPrintEngine.document

val print_variant :
  string -> string -> int -> PPrintEngine.document list -> PPrintEngine.document

val print_tuple : PPrintEngine.document list -> PPrintEngine.document
val constructible_int : (int, int) Monolith.spec
val int : (int, int) Monolith.spec
val positive_int : (int, int) Monolith.spec
val array : ('r, 'c) Monolith.spec -> ('r array, 'c array) Monolith.spec

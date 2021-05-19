val constructible_int : (int, int) Monolith.spec

val positive_int : (int, int) Monolith.spec

val constructible_array :
  'a Monolith.Gen.gen ->
  'a Monolith.printer ->
  ('a array, 'a array) Monolith.spec

val deconstructible_array :
  'a Monolith.printer -> ('a array, 'a array) Monolith.spec

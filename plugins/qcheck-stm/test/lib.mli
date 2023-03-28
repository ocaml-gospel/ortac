type sut

val good_init : unit -> sut
val bad_init : sut
val no_sut : int -> int
val constant : int
val multiple_sut : sut -> sut -> int
val f : sut -> int
val g : int -> sut -> bool

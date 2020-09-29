val f : int -> int [@@gospel {| o = f i
    ensures o = 0 |}]

val g : int -> int

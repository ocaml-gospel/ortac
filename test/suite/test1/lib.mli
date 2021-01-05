val f : int -> int [@@gospel {| o = f i
    ensures o = i + 1 |}]

val g : int -> int
  [@@gospel {| o = g i
    ensures forall j. 0 <= j <= 1 -> o = 0 |}]

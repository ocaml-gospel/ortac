val test_forall : int -> int -> int
  [@@gospel
    {| r = test_forall i j
    requires forall x. i <= x < j -> x > 0 |}]

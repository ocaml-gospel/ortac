val test_forall : int -> int -> int
(*@ r = test_forall i j
    requires forall x. i <= x < j -> x > 0 *)

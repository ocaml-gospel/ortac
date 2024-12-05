type t
(*@ model size : integer
    mutable model contents : char sequence *)

val make : int -> char -> t
(*@ t = make len c
    checks len >= 0
    ensures t.size = len
    ensures t.contents = Sequence.init len (fun j -> c) *)

val map : (char -> char) -> t -> t
(*@ output = map f input
    ensures output.size = input.size
    ensures output.contents = Sequence.init input.size (fun j -> f input.contents[j]) *)

val third_order : (char -> (int -> int) -> char) -> t -> int
(*@ i = third_order f input
    ensures i = 0 *)

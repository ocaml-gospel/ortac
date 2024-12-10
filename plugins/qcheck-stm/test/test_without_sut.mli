type 'a t
(*@ mutable model contents : 'a sequence *)

val make : int -> 'a -> 'a t
(*@ t = make i a
    checks i >= 0
    ensures t.contents = Sequence.init i (fun j -> a) *)

val add : int -> int -> int
(*@ c = add a b
    ensures c = a + b *)

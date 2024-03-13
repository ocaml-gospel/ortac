type 'a t
(*@ mutable model contents : 'a list
    with x
    invariant List.length x.contents > 0 *)

val create : 'a -> 'a t
(*@ t = create a
    ensures t.contents = a :: [] *)

val push : 'a -> 'a t -> unit
(*@ push a t
    modifies t
    ensures t.contents = a :: (old t.contents) *)

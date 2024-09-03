type 'a t
(*@ mutable model contents : 'a sequence
    with x
    invariant Sequence.length x.contents > 0 *)

val create : 'a -> 'a t
(*@ t = create a
    ensures t.contents = Sequence.singleton a *)

val push : 'a -> 'a t -> unit
(*@ push a t
    modifies t
    ensures t.contents = Sequence.cons a (old t.contents) *)

val transfer : 'a t -> 'a t -> unit
(*@ transfer t1 t2
    modifies t1
    modifies t2
    ensures t1.contents = Sequence.singleton (Sequence.hd (old t1.contents))
    ensures t2.contents = Sequence.tl (old t1.contents) ++ (old t2.contents) *)

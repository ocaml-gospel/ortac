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

val copy : 'a t -> 'a t
(*@ r = copy t
    ensures r.contents = t.contents *)

val sub : 'a t -> int -> int -> 'a t
(*@ r = sub t i n
    checks 0 <= i <= Sequence.length t.contents
    checks i <= i + n <= Sequence.length t.contents
    checks n >= 1
    ensures r.contents = if n = 0 then Sequence.empty else t.contents[i..i+n-1] *)

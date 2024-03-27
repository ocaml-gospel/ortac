type 'a t
(*@ mutable model contents : 'a sequence *)

val empty : unit -> 'a t
(*@ t = empty ()
    ensures t.contents = Sequence.empty *)

val push : 'a t -> 'a -> unit
(*@ push t a
    modifies t.contents
    ensures t.contents = Sequence.cons a (old t.contents) *)

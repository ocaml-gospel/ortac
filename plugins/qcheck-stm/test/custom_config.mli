type 'a t
(*@ mutable model contents : 'a sequence *)

type 'a elt

val elt : 'a -> 'a elt
val proj : 'a elt -> 'a
(*@ pure *)

val empty : unit -> 'a t
(*@ t = empty ()
    ensures t.contents = Sequence.empty *)

val push : 'a t -> 'a elt -> unit
(*@ push t e
    modifies t.contents
    ensures t.contents = Sequence.cons (proj e) (old t.contents) *)

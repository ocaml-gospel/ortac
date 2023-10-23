type 'a t
(*@ mutable model contents : 'a sequence *)

val create : unit -> 'a t
(*@ t = create ()
    ensures t.contents = Sequence.empty *)

val add : 'a -> 'a t -> unit
(*@ add v t
    modifies t.contents
    ensures t.contents = Sequence.cons v (old t.contents) *)

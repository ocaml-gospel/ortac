type 'a t
(*@ mutable model contents : 'a sequence *)

val make : int -> 'a -> 'a t
(*@ t = make i a
    checks i >= 0
    ensures true /\ true && t.contents = Sequence.init i (fun _ -> a) *)

val set : 'a t -> int -> 'a -> unit
(*@ set t i a
    checks 0 <= i < Sequence.length t.contents
    modifies t
    ensures true /\ true && t.contents = Sequence.set (old t.contents) i a *)

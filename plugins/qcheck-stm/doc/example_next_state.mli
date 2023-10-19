type 'a t
(*@ model size : int
    mutable model contents : 'a list *)

val make : int -> 'a -> 'a t
(*@ t = make i a
    checks i >= 0
    ensures t.size = i
    ensures t.contents = List.init i (fun j -> a) *)

(* $MDX part-begin=fun-decl *)

val ensures_not_found_for_next_state : 'a t -> unit
(*@ ensures_not_found_for_next_state t
    modifies t.contents
    ensures List.length t.contents = List.length (old t.contents) *)

(* $MDX part-end *)

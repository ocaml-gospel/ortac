type 'a t
(*@ model size : int
    mutable model contents : 'a list *)

val make : int -> 'a -> 'a t
(*@ t = make i a
    checks i >= 0
    ensures t.size = i
    ensures t.contents = List.init i (fun j -> a) *)

val dummy : 'a t -> int
(*@ l = dummy t
    ensures l = t.size *)

(* $MDX part-begin=fun-decl *)

val ghost_arg : char -> 'a t -> bool
(*@ b = ghost_arg [ i : integer] c t *)

(* $MDX part-end *)

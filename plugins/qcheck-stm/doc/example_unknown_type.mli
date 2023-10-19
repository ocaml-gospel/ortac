type 'a t
(*@ model size : int
    mutable model contents : 'a list *)

val make : int -> 'a -> 'a t
(*@ t = make i a
    checks i >= 0
    ensures t.size = i
    ensures t.contents = List.init i (fun j -> a) *)

type new_type

(* $MDX part-begin=fun-decl *)

val type_not_supported : new_type -> 'a t -> new_type
(*@ y = type_not_supported x t *)

(* $MDX part-end *)

type 'a t
(*@ model size : int
    mutable model contents : 'a list *)

val make : int -> 'a -> 'a t
(*@ t = make i a
    checks i >= 0
    ensures t.size = i
    ensures t.contents = List.init i (fun j -> a) *)

(* $MDX part-begin=fun-decl *)

val for_all : 'a t -> bool
(*@ b = for_all t
    ensures b = List.for_all (fun x -> x = x) t.contents *)

(* $MDX part-end *)

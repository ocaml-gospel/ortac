type 'a t
(*@ model size : int
    mutable model contents : 'a list *)

val make : int -> 'a -> 'a t
(*@ t = make i a
    checks i >= 0
    ensures t.size = i
    ensures t.contents = List.init i (fun j -> a) *)

(* $MDX part-begin=fun-decl *)

val f : int -> int -> bool
(*@ b = f x y *)

val compare : 'a t -> 'a t -> bool
(*@ b = compare t1 t2 *)

val of_list : 'a list -> 'a t
(*@ t = of_list xs *)

val g : int * int -> 'a t -> bool
(*@ b = g x t *)

val h : 'a t -> 'a * 'a
(*@ (l, r) = h t *)

val for_all : ('a -> bool) -> 'a t -> bool
(*@ b = for_all p t *)

(* $MDX part-end *)

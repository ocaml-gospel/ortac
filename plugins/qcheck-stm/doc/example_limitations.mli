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

val of_list : 'a list -> 'a t
(*@ t = of_list xs *)

val g : 'a t -> 'a * 'a * 'a * 'a * 'a * 'a * 'a * 'a * 'a * 'a -> bool
(*@ b = g t x *)

val for_all : ('a -> bool) -> 'a t -> bool
(*@ b = for_all p t *)

(* $MDX part-end *)

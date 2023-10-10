type 'a t
(*@ model size : int
    mutable model contents : 'a list *)

val make : int -> 'a -> 'a t
(*@ t = make i a
    checks i >= 0
    ensures t.size = i
    ensures t.contents = List.init i (fun j -> a) *)

(* $MDX part-begin=fun-decl *)

val unsupported_quantification : 'a t -> bool
(*@ b = unsupported_quantification t
    ensures b = forall a. List.mem a t.contents -> a = a *)

(* $MDX part-end *)

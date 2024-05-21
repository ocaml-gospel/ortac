type ('a, 'b) t
(*@ mutable model m : 'a -> 'b option *)

val empty : unit -> ('a, 'b) t
(*@ t = empty ()
    ensures t.m = fun _ -> None *)

val add : ('a, 'b) t -> 'a -> 'b -> unit
(*@ add t a b
    modifies t.m
    ensures t.m = fun x -> if x = a then Some b else (old t.m) x *)

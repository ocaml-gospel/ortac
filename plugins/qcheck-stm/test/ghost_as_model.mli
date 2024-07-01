(*@ type m = A of integer *)

type t
(*@ mutable model m : m *)

val create : unit -> t
(*@ t = create ()
    ensures t.m = A 0 *)

val use : t -> unit
(*@ use t
    modifies t.m
    ensures t.m = match old t.m with A x -> A (succ x) *)

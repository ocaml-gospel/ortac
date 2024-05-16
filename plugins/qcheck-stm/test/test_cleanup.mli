type t
(*@ mutable model m : integer *)

val create : unit -> t
(*@ t = create ()
    ensures t.m = 0 *)

val use : t -> unit
(*@ use t
    modifies t.m
    ensures t.m = 1 + old t.m *)

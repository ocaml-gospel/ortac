type t
(*@ mutable model i : integer *)

val create : unit -> t
(*@ t = create ()
    ensures t.i = 0 *)

val use : t -> unit
(*@ use t
    modifies t.i
    ensures t.i = succ (old t.i) *)

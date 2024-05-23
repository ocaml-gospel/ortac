(*@ type m = A of integer *)

type t
(*@ mutable model view : m *)

val create : unit -> t
(*@ t = create ()
    ensures t.view = A 0 *)

val use : t -> unit
(*@ use t
    modifies t.view
    ensures t.view = match old t.view with A x -> A (succ x) *)

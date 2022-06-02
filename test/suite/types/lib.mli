type t = private { x : int; y : bool }
(*@ with t
    invariant t.y = false *)

val v : int -> bool -> t
(*@ t = v x y
    ensures x = t.x
    ensures y = t.y *)

val e : t
(*@ ensures e.x >= 0 *)

val get_x : t -> int
(*@ r = get_x t
    ensures r = t.x *)

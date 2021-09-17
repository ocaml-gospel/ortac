type t = { x: int; y: bool }

val e : t
(*@ ensures e.x >= 0 *)

val get_x : t -> int
(*@ r = get_x t
    ensures r = t.x *)

type t = { x: int; y: bool }

val get_x : t -> int
(*@ r = get_x t
    ensures r = t.x *)

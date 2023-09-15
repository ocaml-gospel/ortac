type t = private int ref
(*@ mutable model value : integer *)

val make : int -> t
(*@ r = make i
    ensures r.value = i *)

val get : t -> int
(*@ i = get r
    pure
    ensures i = r.value
    ensures i + 1 = succ !r *)

type t = private { c : int }
(*@ model value : integer
    with x invariant integer_of_int x.c = x.value *)

val make : int -> t
(*@ r = make i
    ensures r.value = i *)

val get : t -> int
(*@ i = get r
    pure
    ensures i = r.value
    ensures i = r.c *)

type t = private { c : int }
(*@ model value : integer
    with x invariant integer_of_int x.c = x.value *)

val make : int -> t
(*@ r = make i
    ensures r.value = i *)

val plus1 : int -> int
(*@ i1 = plus1 i
    ensures i1 = i + 1 *)

(*@ function plus1 (i: integer) : integer = i + 1 *)

val plus2 : int -> int
(*@ i2 = plus2 i
    pure
    ensures i2 = i + 2 *)

val get : t -> int
(*@ i = get r
    pure
    ensures i = r.value
    ensures i = r.c
    ensures plus1 i = i + 1
    ensures plus2 i = i + 2 *)

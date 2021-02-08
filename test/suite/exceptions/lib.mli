val raise_oom : int -> int
(*@ o = raise_oom i *)

val raise_stackoverflow : int -> int
(*@ o = raise_stackoverflow i *)

val undeclared_raise_notfound : int -> int
(*@ o = undeclared_raise_notfound i *)

val bad_raise_notfound : int -> int
(*@ o = bad_raise_notfound i
    raises Not_found -> false *)

val raise_notfound : int -> int
(*@ o = raise_notfound i
    raises Not_found -> true *)

val raise_invalidarg : string -> int
(*@ o = raise_invalidarg i
    raises Invalid_argument s -> i = s | Invalid_argument _ -> false
    raises Invalid_argument _ -> true *)

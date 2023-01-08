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
    raises Not_found -> true
    raises Invalid_argument s -> i = s | Invalid_argument _ -> false
    raises Invalid_argument _ -> true *)

val check : bool -> bool
(*@ y = check x
    checks x = true *)

val double_check : int -> int
(*@ y = double_check x
    checks x > 0
    checks x <= 10*)

val bad_check_modifies : bool ref -> bool
(*@ y = bad_check_modifies x
    modifies x
    checks !x = true *)

val bad_check : bool -> bool
(*@ y = bad_check x
    checks x = true *)

val bad_check2 : bool -> bool
(*@ y = bad_check2 x
    checks x = true *)

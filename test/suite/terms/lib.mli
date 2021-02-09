(* && and || are lazy, but /\ and \/ are not *)

val lazy_bool : int -> int
(*@ y = lazy_bool x
    requires x = x || 1/0 = 2
    requires not (x <> x && 1/0 = 2) *)

val not_lazy_or : int -> int
(*@ y = not_lazy_or x
    requires x = x \/ 1/0 = 2 *)

val not_lazy_and : int -> int
(*@ y = not_lazy_and x
    requires not (x <> x /\ 1/0 = 2) *)

(* variable scope *)

val scope1 : int -> int
(*@ y = scope1 x
    requires let x = true in x
    ensures  let y = true in y *)

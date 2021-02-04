(* && and || are lazy, but /\ and \/ are not *)

val lazy_bool : int -> int
  [@@gospel
    {| y = lazy_bool x
       requires x = x || 1/0 = 2
       requires not (x <> x && 1/0 = 2) |}]

val not_lazy_bool1 : int -> int
  [@@gospel {| y = not_lazy_bool1 x
    requires x = x \/ 1/0 = 2 |}]

val not_lazy_bool2 : int -> int
  [@@gospel {| y = not_lazy_bool2 x
    requires not (x <> x /\ 1/0 = 2) |}]

(* variable scope *)

val scope1 : int -> int
  [@@gospel
    {| y = scope1 x
       requires let x = true in x
       ensures  let y = true in y |}]

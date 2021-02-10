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

(* Terms and formulas *)

val if_forall : int -> int
(*@ y = if_forall x
    requires if forall i. 0 <= i < 10 -> x <> i then x = 10 else x = 3 *)

val equiv : unit -> unit
(*@ equiv ()
    ensures (1 = 2) <-> (2 = 3) *)

val exists_ : unit -> unit
(*@ exists_ ()
    ensures exists x. 0 <= x < 10 /\ x = 3 *)

(* Pattern matching *)

type t = A | B of string

val a : t -> unit
(*@ a x
    requires match x with
            | A -> true
            | B s -> false
    requires x = A *)

val b : t -> unit
(*@ b x
    requires match x with
            | A -> false
            | B _ -> true *)

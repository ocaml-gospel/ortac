type set = private { size : int; mutable mask : int }
(*@ with t
    invariant 0 <= t.size <= 32
    invariant 0 <= t.mask < pow 2 t.size *)

(*@ predicate mem (i: integer) (bv: set) =
      logand bv.mask (pow 2 i) <> 0 *)

val create : int -> set
(*@ bv = create n
    requires 0 <= n <= 32
    ensures bv.size = n
    ensures forall i. 0 <= i < n -> not (mem i bv) *)

val add : int -> set -> unit
(*@ add i bv
    requires 0 <= i < bv.size
    modifies bv.mask
    ensures forall j. 0 <= j < bv.size ->
                mem j bv <-> i = j \/ mem j (old bv) *)

val mem : int -> set -> bool
(*@ b = mem i bv
    requires 0 <= i < bv.size
    ensures b <-> mem i bv *)

type t = private { value : int }
(*@ with t
    invariant t.value >= 0 *)

exception Int_overflow

val create_int : int -> t
(*@ r = create_int n
    requires n >= 0
    ensures  r.value = n *)

(* create_int without:
    - [mli] requires n >= 0 to reach `violated invariant`
    - [ml]  failwith if n < 0 to avoid `unexpected exception`
*)
val bad_create_int : int -> t
(*@ r = bad_create_int n
    ensures  r.value = n *)

val increment_int : t -> int
(*@ r = increment_int x
    checks   x.value >= 0
    requires x.value < max_int
    ensures  r = x.value + 1
    raises   Int_overflow -> true *)

(* increment_int without:
    - [mli] requires x.value < max_int to reach `Int_overflow`
*)
val bad_increment_int : t -> int
(*@ r = bad_increment_int x
    checks   x.value >= 0
    ensures  r = x.value + 1
    raises   Int_overflow -> true *)

(* increment_int with:
    - [mli] checks x.value == 1 to reach the hidden `Invalid_argument`
*)
val bad2_increment_int : t -> int
(*@ r = bad2_increment_int x
    checks   x.value <> 1
    requires x.value < max_int
    ensures  r = x.value + 1
    raises   Int_overflow -> true *)

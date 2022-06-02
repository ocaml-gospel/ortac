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

type 'a t
(*@ model capacity: int
    mutable model view: 'a list
    with t
    invariant t.capacity > 0
    invariant List.length t.view <= t.capacity *)

val capacity : 'a t -> int [@@model]
val view : 'a t -> 'a list [@@model]
val create : int -> 'a t
(*@ t = create c
    requires c > 0
    ensures t.capacity = c
    ensures t.view = [] *)

val is_empty : 'a t -> bool
(*@ b = is_empty t
    pure
    ensures b <-> t.view = [] *)

val mem : 'a t -> 'a -> bool
(*@ b = mem t x
    pure
    ensures b <-> List.mem x t.view *)

val clear : 'a t -> unit
(*@ clear t
    modifies t.view
    ensures is_empty t *)

val add : 'a t -> 'a -> unit
(*@ add t x
    modifies t.view
    ensures t.view = x :: (old t.view) *)

val tail : 'a t -> unit
(*@ tail t
    modifies t.view
    requires t.view <> []
    ensures t.capacity = old t.capacity - 1
    ensures t.view = List.tl (old t.view) *)

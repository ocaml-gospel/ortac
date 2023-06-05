type 'a t
(*@ model size : int
    mutable model contents : 'a list *)

val length : 'a t -> int
(*@ i = length t
    ensures i = t.size *)

val get : 'a t -> int -> 'a
(*@ a = get t i
    checks 0 <= i < t.size
    ensures a = List.nth t.contents i *)

val set : 'a t -> int -> 'a -> unit
(*@ set t i a
    checks 0 <= i < t.size
    modifies t.contents
    ensures t.contents = List.mapi (fun j x -> if j = (i : integer) then a else x) (old t.contents) *)

val make : int -> 'a -> 'a t
(*@ t = make i a
    checks i >= 0
    ensures t.size = i
    ensures t.contents = List.init i (fun j -> a) *)

val fill : 'a t -> int -> int -> 'a -> unit
(*@ fill t i j a
    checks 0 <= i
    checks 0 <= j
    checks i + j <= t.size
    modifies t.contents
    ensures t.contents = List.mapi (fun k x -> if i <= k < i+j then a else x) (old t.contents) *)

val to_list : 'a t -> 'a list
(*@ l = to_list t
    ensures l = t.contents *)

val mem : 'a -> 'a t -> bool
(*@ b = mem a t
    ensures b = List.mem a t.contents *)

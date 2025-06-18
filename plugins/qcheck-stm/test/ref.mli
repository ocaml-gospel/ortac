type 'a t
(*@ mutable model value : 'a *)

val make : 'a -> 'a t
(*@ r = make v
    ensures r.value = v *)

val get : 'a t -> 'a
(*@ v = get r
    pure
    ensures v = r.value *)

val set : 'a t -> 'a -> unit
(*@ set r v
    modifies r.value
    ensures r.value = v *)

val incr : int t -> unit
(*@ incr r
    modifies r.value
    ensures r.value = succ (old r.value) *)

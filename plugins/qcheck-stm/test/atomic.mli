type !'a t
(*@ mutable model content : 'a *)

val make : 'a -> 'a t
(*@ r = make v
    ensures r.content = v *)

val get : 'a t -> 'a
(*@ v = get r
    ensures v = r.content *)

val set : 'a t -> 'a -> unit
(*@  set r v
     modifies r.content
     ensures r.content = v *)

val exchange : 'a t -> 'a -> 'a
(*@ res = exchange r v
    modifies r.content
    ensures r.content = v
    ensures res = old r.content *)

val compare_and_set : 'a t -> 'a -> 'a -> bool
(*@ b = compare_and_set r seen v
    modifies r
    ensures r.content = if r.content = seen then v else old r.content
    ensures b <-> old r.content = seen *)

val fetch_and_add : int t -> int -> int
(*@ res = fetch_and_add r n
    modifies r.content
    ensures r.content = old r.content + n
    ensures res = old r.content *)

val incr : int t -> unit
(*@ incr r
    modifies r.content
    ensures r.content = old r.content + 1 *)

val decr : int t -> unit
(*@ decr r
    modifies r.content
    ensures r.content = old r.content - 1 *)

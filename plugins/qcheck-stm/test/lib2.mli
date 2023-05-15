type 'a t
(*@ model size : integer
    model contents : 'a List.t *)

val empty : int -> 'a t
(*@ t = empty i
    checks i > 0
    ensures t.size = i
    ensures t.contents = [] *)

val length : 'a t -> int
(*@ i = length t
    ensures i = List.length t.contents *)

val pop : 'a t -> 'a
(*@ a = pop t
    ensures a = List.hd (old t.contents)
    modifies t.contents
    modifies t.contents
    ensures t.contents = List.tl (old t.contents) *)

val push : 'a -> 'a t -> unit
(*@ push a t
    modifies t.contents
    ensures t.contents = if List.length (old t.contents) = (old t.size)
                         then (old t.contents)
                         else (a :: (old t.contents)) *)

val extend : 'a t -> unit
(*@ extend t
    modifies t.size
    modifies t.contents
    ensures t.contents = []
    ensures t.size = 2 * old t.size *)

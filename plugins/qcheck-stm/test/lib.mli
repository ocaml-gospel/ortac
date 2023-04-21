type ('a, 'b) t
(*@ model fst : 'a
    model snd : 'b *)

val make : 'a -> 'b -> ('a, 'b) t
val f : 'a -> ('a, 'b) t -> bool
val g : 'a -> ('b, 'a) t -> int
val h : ('a, 'b) t -> ('b, 'a) t -> bool
val i : (int, int) t -> bool
val j : (int, char) t -> 'a -> bool

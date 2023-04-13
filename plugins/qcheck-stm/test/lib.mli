type ('a, 'b) t

val make : 'a -> 'b -> ('a, 'b) t
val f : 'a -> ('a, 'b) t -> bool
val g : 'a -> ('b, 'a) t -> int
val h : ('a, 'b) t -> ('b, 'a) t -> bool
val i : (int, int) t -> bool
val j : (char, int) t -> 'a -> bool

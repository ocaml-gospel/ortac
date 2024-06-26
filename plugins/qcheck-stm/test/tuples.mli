type (!'a, !'b) t
(*@ mutable model contents : ('a * 'b) list *)

val create : unit -> ('a, 'b) t
(*@ h = create ()
    ensures h.contents = [] *)

val clear : ('a, 'b) t -> unit
(*@ clear h
    modifies h
    ensures h.contents = [] *)

val add : ('a, 'b) t -> 'a * 'b -> unit
(*@ add h tup
    modifies h
    ensures h.contents = match tup with a, b -> (a, b) :: old h.contents *)

val add' : ('a, 'b) t -> bool * 'a * 'b -> unit
(*@ add' h tup
    modifies h
    ensures h.contents = match tup with c, a, b ->
        if c then (a, b) :: old h.contents
        else old h.contents *)

val add'' : ('a, 'b) t -> bool * ('a * 'b) -> unit
(*@ add'' h tup
    modifies h
    ensures h.contents = match tup with c, (a, b) ->
        if c then (a, b) :: old h.contents
        else old h.contents *)

val size_tup : ('a, 'b) t -> int * int
(*@ x, y = size_tup t
    ensures x = List.length t.contents
    ensures y = List.length t.contents *)

val size_tup' : ('a, 'b) t -> int * int * int
(*@ x, y, z = size_tup' t
    ensures x = List.length t.contents
    ensures y = List.length t.contents
    ensures z = List.length t.contents *)

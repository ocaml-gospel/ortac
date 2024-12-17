type 'a t
(*@ mutable model value : 'a *)

val make : 'a -> 'a t
(*@ t = make a
    ensures t.value = a *)

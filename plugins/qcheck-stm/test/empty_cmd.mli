type t
(*@ model content : integer *)

val make : unit -> t
(*@ t = make u
    ensures t.content = 0 *)

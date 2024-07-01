type t = A of int ref

let create () = A (ref 0)
let use (A x) = incr x

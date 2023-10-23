type 'a t = 'a list ref

let create () = ref []
let add x s = s := x :: !s

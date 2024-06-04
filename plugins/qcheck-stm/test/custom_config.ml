type 'a elt = Elt of 'a

let elt a = Elt a

type 'a t = 'a elt list ref

let proj = function Elt a -> a
let empty () = ref []
let push t e = t := e :: !t
let top t = match !t with [] -> invalid_arg "top" | x :: _ -> x

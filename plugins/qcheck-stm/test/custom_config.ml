type 'a t = 'a list ref
let empty () = ref []
let push t a = t := a :: !t

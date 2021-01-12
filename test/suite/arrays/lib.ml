let create = Array.make

let bad_create n _ = Array.make n 42

let get = Array.get

let set = Array.set

let bad_get a i = a.(i + 1)

let fill = Array.fill

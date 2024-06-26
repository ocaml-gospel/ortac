include Hashtbl

let create () = create ~random:false 10
let add t (a, b) = add t a b
let add' t (c, a, b) = if c then add t (a, b) else ()
let add'' t (c, (a, b)) = if c then add t (a, b) else ()
let size_tup t = (length t, length t)
let size_tup' t = (length t, length t, length t)

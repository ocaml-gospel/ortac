type ('a, 'b) t = ('a, 'b) Hashtbl.t

let empty () = Hashtbl.create 42
let add = Hashtbl.add

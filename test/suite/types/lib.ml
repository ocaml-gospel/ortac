type t = { x : int; y : bool }

let v x y = { x; y }
let e = { x = 0; y = false }
let get_x t = t.x

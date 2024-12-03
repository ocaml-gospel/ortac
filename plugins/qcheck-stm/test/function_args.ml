type t = char array

let make l (c : char) = Array.make l c
let map = Array.map

let third_order f t =
  let _ = map (fun c -> f c Fun.id) t in
  0

let arity_over_four f t =
  map (fun c -> f c c c c c) t |> Array.fold_left ( + ) 0

type 'a t = 'a ref

let make = ref
let get r = !r
let set r v = r := v
let incr = incr

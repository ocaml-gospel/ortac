type set = { size : int; mutable mask : int }

let create n = { size = n; mask = 0 }
let add i s = s.mask <- s.mask lor (1 lsl i)
let mem i s = s.mask lor (1 lsl i) <> 0

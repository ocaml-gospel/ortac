type 'a t = { cap : int; mutable elements : 'a list }

let capacity t = t.cap
let view t = t.elements
let length t = List.length t.elements
let create n = { cap = n; elements = [] }
let is_empty t = t.elements = []
let mem t x = List.mem x t.elements
let clear t = t.elements <- []
let add t x = if length t + 1 <= t.cap then t.elements <- x :: t.elements
let tail t = t.elements <- List.tl t.elements

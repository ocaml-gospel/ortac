type 'a t = 'a list

let make n a = List.init n (fun _ -> a)
let sut_in_type = List.length
let sut_in_tuple _ = true
let sut_in_nested_type _ = true
let sut_nested_in_tuple _ = true

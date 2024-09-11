type 'a t = 'a list ref

let create a = ref [ a ]
let push a t = t := a :: !t

let transfer t1 t2 =
  t1 := [ List.hd !t1 ];
  t1 := List.tl !t1 @ !t2

let copy t = ref !t

let sub t i n =
  if i < 0 || n < 1 || n > List.length !t || i + n > List.length !t then
    raise (Invalid_argument "sub")
  else ref (List.filteri (fun idx _ -> i <= idx && idx <= i + n) !t)

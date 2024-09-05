type 'a t = 'a list ref

let create a = ref [ a ]
let push a t = t := a :: !t

let transfer t1 t2 =
  t1 := [ List.hd !t1 ];
  t1 := List.tl !t1 @ !t2

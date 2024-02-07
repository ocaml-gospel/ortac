type 'a t = 'a list ref

let create a = ref [ a ]
let push a t = t := a :: !t

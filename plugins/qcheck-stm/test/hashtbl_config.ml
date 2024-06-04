open Hashtbl

let init_sut = create ~random:false 16

type sut = (char, int) t

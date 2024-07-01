open Test_cleanup

let init_sut = create ()

type sut = t

let cleanup t = ignore t

open Queue

let init_sut = create ()

type sut = int t

module Gen = struct
  let int = small_signed_int
end

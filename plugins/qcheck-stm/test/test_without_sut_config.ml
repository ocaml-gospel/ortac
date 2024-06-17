open Test_without_sut

type sut = int t

let init_sut = make 16 0

module Gen = struct
  let int = small_signed_int
end

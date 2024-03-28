open Example

type sut = char t

let init_sut = make 42 'a'

module Gen = struct
  let int = small_signed_int
end

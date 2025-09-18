open Array

let init_sut = make 16 'a'

type sut = char t

module Gen = struct
  let int = small_signed_int
end

module Frequencies = struct
  let make = 0
end

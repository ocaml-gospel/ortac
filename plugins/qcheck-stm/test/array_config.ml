open Array

let init_sut = make 16 'a'

type sut = char t

module Gen = struct
  let int = nat_small
end

module Weights = struct
  let make = 0
end

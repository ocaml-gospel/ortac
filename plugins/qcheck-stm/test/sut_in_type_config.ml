open Sut_in_type

type sut = int t

let init_sut = make 16 0

module Gen = struct
  let int = nat_small
end

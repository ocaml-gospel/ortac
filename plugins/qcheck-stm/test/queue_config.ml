open Queue

let init_sut = create ()

type sut = int t

module Gen = struct
  let int = nat_small
end

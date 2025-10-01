type sut = int t

let init_sut = create ()

module Gen = struct
  let int = nat
  let list = small_list
end

type sut = int t

let init_sut = make 42

module Gen = struct
  (* to avoid overflow *)
  let int = small_int
end

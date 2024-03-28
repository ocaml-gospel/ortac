type sut = int t
let init_sut = empty ()
module Gen = struct
  let int = small_signed_int
end
module Pp = struct
  let _pp_wont_be_used = of_show (fun _ -> "dummy") false
end

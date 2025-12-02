type sut = int t

let init_sut = create ()

module Gen = struct
  let int = nat
  let list = small_list
end

module Frequencies_dom1 = struct
  let create = 0
  let of_list = 0
  let is_empty = 0
  let close = 0
  let pop_exn = 0
  let pop_opt = 0
  let drop_exn = 0
  let peek_exn = 0
  let peek_opt = 0
  let push_head = 0
end

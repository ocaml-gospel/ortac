open Varray

type sut = char t

let init_sut = make 42 'a'

module Pp = struct
  let pp_elt pp = pp
end

module Gen = struct
  let int = small_signed_int
  let elt gen = gen
end

module Ty = struct
  type _ ty += Elt : 'a ty -> 'a elt ty

  let elt spec =
    let ty, show = spec in
    (Elt ty, fun x -> Printf.sprintf "Elt %s" (show x))
end

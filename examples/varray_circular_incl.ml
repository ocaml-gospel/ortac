module Util = struct
  module Pp = struct
    include Util.Pp

    let pp_elt pp = pp
  end
end

module QCheck = struct
  include QCheck

  module Gen = struct
    include QCheck.Gen

    let int = small_signed_int
    let elt gen = gen
  end
end

open STM
open Varray_circular_spec

type _ ty += Elt : 'a ty -> 'a elt ty

let elt spec =
  let ty, show = spec in
  (Elt ty, fun x -> Printf.sprintf "Elt %s" (show x))

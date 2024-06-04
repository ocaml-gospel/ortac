type sut = int t

let init_sut = empty ()

module Gen = struct
  let int = small_signed_int
  let elt gen = elt <$> gen
end

module Pp = struct
  let pp_elt pp par fmt e =
    let open Format in
    fprintf fmt "(Elt %a)" (pp par) (proj e)
end

module Ty = struct
  type _ ty += Elt : 'a ty -> 'a elt ty

  let elt spec =
    let ty, show = spec in
    (Elt ty, fun x -> Printf.sprintf "Elt %s" (show (proj x)))
end

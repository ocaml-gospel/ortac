type error =
  | BadPost of { loc : Ppxlib.location; fun_name : string; term : string }

open Fmt

let styled_list l pp = List.fold_left (fun acc x -> styled x acc) pp l

let pp_loc = styled `Bold Ppxlib.Location.print

let report ppf = function
  | BadPost { loc; fun_name; term } ->
      pf ppf
        "%a@\n\
        \ %a: Gospel specification unmet in function %a:@\n\
        \ %a %a is violated" pp_loc loc
        (styled `Red string)
        "Runtime error"
        (styled `Yellow string)
        fun_name
        (styled_list [ `Blue; `Underline ] string)
        "post-condition"
        (styled `Bold string)
        term

exception Error of error

module Z = struct
  include Z

  let rec forall start stop p =
    if equal start stop then p start else p start && forall (succ start) stop p
end

module Array = struct
  let create z =
    if Z.(z > of_int Sys.max_array_length) then
      raise (Invalid_argument "Array length too big")
    else Array.make (Z.to_int z)

  let get arr z =
    if Z.(z < zero || z >= of_int (Array.length arr)) then
      raise (Invalid_argument "Out of array bounds")
    else Array.unsafe_get arr (Z.to_int z)

  let length arr = Array.length arr |> Z.of_int
end

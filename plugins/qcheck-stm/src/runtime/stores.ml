module Model = struct
  module Make (M : sig
    type elt

    val init : elt
  end) =
  struct
    type elt = M.elt
    type t = elt list

    let create n () : t = List.init n (fun _ -> M.init)
    let size = List.length
    let rec drop_n t n = if n = 0 then t else drop_n (List.tl t) (n - 1)
    let push t e = e :: t

    let get t n =
      try List.nth t n with _ -> failwith ("nth: " ^ string_of_int n)
  end
end

module SUT = struct
  module Make (M : sig
    type sut

    val init : unit -> sut
  end) =
  struct
    type elt = M.sut
    type t = elt list ref

    let create n () = ref @@ List.init n (fun _ -> M.init ())
    let size t = List.length !t
    let get t = List.nth !t
    let push t e = t := e :: !t
    let get_name t n = Format.asprintf "sut%d" (size t - n - 1)
  end
end

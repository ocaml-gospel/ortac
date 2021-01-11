open Fmt

type term = Pre of string | Post of string

type error_kind = Violated | RuntimeExn of exn

type error =
  | Condition of {
      loc : Ppxlib.location;
      fun_name : string;
      term : term;
      error_kind : error_kind;
    }

let styled_list l pp = List.fold_left (fun acc x -> styled x acc) pp l

let pp_kind ppf = function
  | Violated -> (styled `Yellow string) ppf "violated"
  | RuntimeExn e ->
      pf ppf "%a:@ %a"
        (styled `Yellow string)
        "not executable"
        (styled `Bold string)
        (Printexc.to_string e)

let pp_term =
  using
    (function Pre t -> ("pre-condition", t) | Post t -> ("post-condition", t))
    (fun ppf (p, t) ->
      pf ppf "%a @[%a@]"
        (styled_list [ `Yellow; `Underline ] string)
        p
        (styled `Bold string)
        t)

let pp_loc = styled `Bold Ppxlib.Location.print

let report ppf = function
  | Condition { loc; fun_name; term; error_kind } ->
      pf ppf "%a@\n %a: Gospel specification unmet in function %a:@\n %a is %a"
        pp_loc loc
        (styled `Red string)
        "Runtime error"
        (styled `Blue string)
        fun_name
        (styled `Bold pp_term)
        term
        (styled `Yellow pp_kind)
        error_kind

exception Error of error

let runtime_exn loc fun_name term exn =
  raise (Error (Condition { loc; fun_name; term; error_kind = RuntimeExn exn }))

let violated loc fun_name term =
  raise (Error (Condition { loc; fun_name; term; error_kind = Violated }))

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

open Ppxlib

type level = Warning | Error
type kind = ..
type kind += Unsupported of string

exception Unkown_kind

type t = kind * Location.t

let level = function Unsupported _ -> Warning | _ -> raise Unkown_kind

exception Error of t

open Fmt

let styled_list l pp = List.fold_left (fun acc x -> styled x acc) pp l

let pp_level ppf = function
  | Warning -> pf ppf "%a: " (styled_list [ `Yellow; `Bold ] string) "Warning"
  | Error -> pf ppf "%a: " (styled_list [ `Red; `Bold ] string) "Error"

let quoted ppf = pf ppf "`%s'"

let pp_kind ppf = function
  | Unsupported msg ->
      pf ppf "unsupported %s. The clause has not been translated" msg
  | _ -> raise Unkown_kind

let is_fake_loc loc = loc.loc_start.pos_fname = "_none_"

let pp_param pp_kind level ppf (k, loc) =
  if is_fake_loc loc then pf ppf "%a@[%a@]@\n" pp_level (level k) pp_kind k
  else
    pf ppf "%a@\n%a@[%a@]@\n"
      (styled `Bold Location.print)
      loc pp_level (level k) pp_kind k

let pp = pp_param pp_kind level

let () =
  Printexc.register_printer (function
    | Error t -> Some (Fmt.str "%a" pp t)
    | _ -> None)

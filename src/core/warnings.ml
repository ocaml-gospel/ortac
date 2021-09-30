open Ppxlib

type level = Warning | Error

type kind = Unsupported of string

type t = kind * Location.t

let level = function Unsupported _ -> Warning

exception Error of t

let w = ref []

let register t = w := t :: !w

open Fmt

let styled_list l pp = List.fold_left (fun acc x -> styled x acc) pp l

let pp_level ppf = function
  | Warning -> pf ppf "%a: " (styled_list [ `Yellow; `Bold ] string) "Warning"
  | Error -> pf ppf "%a: " (styled_list [ `Red; `Bold ] string) "Error"

let pp_kind ppf = function
  | Unsupported msg ->
      pf ppf "unsupported %s@\nthe clause has not been translated" msg

let pp ppf (k, loc) =
  pf ppf "%a@\n%a@[%a@]"
    (styled `Bold Location.print)
    loc pp_level (level k) pp_kind k

let report () = if !w <> [] then epr "%a@." (list ~sep:(any "@\n") pp) !w

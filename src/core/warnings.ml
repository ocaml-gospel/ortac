open Ppxlib

type level = Warning | Error

type kind =
  | Unsupported of string
  | MonolithSpec of string
  | MonolithPrinter of string
  | MonolithGen of string

type t = kind * Location.t

let level = function
  | Unsupported _ | MonolithSpec _ | MonolithPrinter _ | MonolithGen _ ->
      Warning

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
  | MonolithSpec msg ->
      pf ppf "unsupported %s Monolith spec@\nthe test has not been declared" msg
  | MonolithPrinter msg ->
      pf ppf
        "unsupported %s Monolith printer@\n\
         the Monolith program may not work as expected" msg
  | MonolithGen msg ->
      pf ppf
        "unsupported %s Monolith generator@\n\
         the Monolith program may not work as expected" msg

let pp ppf (k, loc) =
  pf ppf "%a@\n%a@[%a@]"
    (styled `Bold Location.print)
    loc pp_level (level k) pp_kind k

let report () = epr "%a@." (list ~sep:(any "@\n") pp) !w

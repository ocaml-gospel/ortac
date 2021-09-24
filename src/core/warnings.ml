open Ppxlib

type level = Warning | Error

type kind =
  | Unsupported of string
  | Unsupported_old_copy of string
  | Unsupported_old_use of string
  | Unsupported_model of string
  | Unsupported_model_use of string

type t = kind * Location.t

let level = function _ -> Warning

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
  | Unsupported_old_copy msg ->
      pf ppf "unable to copy the variable `%s' in the pre-state" msg
  | Unsupported_old_use msg ->
      pf ppf
        "use of unsupported old variable `%s'@\n\
         the clause has not been translated" msg
  | Unsupported_model model ->
      pf ppf "models are not supported@\nthe model `%s' has not been translated"
        model
  | Unsupported_model_use model ->
      pf ppf
        "this term uses the model `%s', which is not supported@\n\
         the clause was not translated" model

let pp ppf (k, loc) =
  pf ppf "%a@\n%a@[%a@]"
    (styled `Bold Location.print)
    loc pp_level (level k) pp_kind k

let report () = epr "%a@." (list ~sep:(any "@\n") pp) !w

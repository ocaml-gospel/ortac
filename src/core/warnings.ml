open Ppxlib

type level = Warning | Error
type kind = ..

type kind +=
  | Unsupported of string
  | Ghost_value of string
  | Ghost_type of string
  | Unsupported_model of string * string
  | Function_without_definition of string
  | Predicate_without_definition of string

exception Unkown_kind

type t = kind * Location.t

let level = function
  | Unsupported _ | Ghost_value _ | Ghost_type _ | Unsupported_model _
  | Function_without_definition _ | Predicate_without_definition _ ->
      Warning
  | _ -> raise Unkown_kind

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
  | Ghost_value name ->
      pf ppf "%a is a ghost value. It was not translated." quoted name
  | Ghost_type name ->
      pf ppf "%a is a ghost type. It was not translated." quoted name
  | Unsupported_model (type_, name) ->
      pf ppf "Model %a of type %a is not supported. It was not translated."
        quoted name quoted type_
  | Function_without_definition name ->
      pf ppf "The function %a has no definition. It was not translated." quoted
        name
  | Predicate_without_definition name ->
      pf ppf "The predicate %a has no definition. It was not translated." quoted
        name
  | _ -> raise Unkown_kind

let is_fake_loc loc = loc.loc_start.pos_fname = "_none_"

let pp_param pp_kind level ppf (k, loc) =
  if is_fake_loc loc then pf ppf "%a@[%a@]@\n" pp_level (level k) pp_kind k
  else
    pf ppf "%a@\n%a@[%a@]@\n"
      (styled `Bold Location.print)
      loc pp_level (level k) pp_kind k

let pp = pp_param pp_kind level

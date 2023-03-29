module W = Ortac_core.Warnings

type W.kind +=
  | Value_is_a_constant of string
  | Value_return_sut of string
  | Value_have_no_sut_argument of string
  | Value_have_multiple_sut_arguments of string

let level kind =
    match kind with
    | Value_is_a_constant _ | Value_return_sut _ | Value_have_no_sut_argument _
    | Value_have_multiple_sut_arguments _ ->
        W.Warning
    | _ -> W.level kind

type 'a reserr = ('a, W.t list) result

let ok = Result.ok
let error e = Result.error [ e ]
let ( let* ) = Result.bind

let ( and* ) a b =
  match (a, b) with
  | Error e0, Error e1 -> Error (e0 @ e1)
  | Error e, _ | _, Error e -> Error e
  | Ok a, Ok b -> Ok (a, b)

open Fmt

let pp_kind ppf kind =
    match kind with
    | Value_is_a_constant id -> pf ppf "%a is a constant." W.quoted id
    | Value_return_sut id -> pf ppf "%a returns a sut." W.quoted id
    | Value_have_no_sut_argument id ->
        pf ppf "%a have no sut argument." W.quoted id
    | Value_have_multiple_sut_arguments id ->
        pf ppf "%a have multiple sut arguments." W.quoted id
    | _ -> W.pp_kind ppf kind

let pp_errors = W.pp_param pp_kind level |> list
let pp pp_ok = Fmt.result ~ok:pp_ok ~error:pp_errors

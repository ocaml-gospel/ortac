module W = Ortac_core.Warnings

type W.kind +=
  | Constant_value of string
  | Returning_sut of string
  | No_sut_argument of string
  | Multiple_sut_arguments of string

let level kind =
  match kind with
  | Constant_value _ | Returning_sut _ | No_sut_argument _
  | Multiple_sut_arguments _ ->
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
  | Constant_value id -> pf ppf "%a is a constant." W.quoted id
  | Returning_sut id -> pf ppf "%a returns a sut." W.quoted id
  | No_sut_argument id -> pf ppf "%a have no sut argument." W.quoted id
  | Multiple_sut_arguments id ->
      pf ppf "%a have multiple sut arguments." W.quoted id
  | _ -> W.pp_kind ppf kind

let pp_errors = W.pp_param pp_kind level |> list
let pp pp_ok = Fmt.result ~ok:pp_ok ~error:pp_errors

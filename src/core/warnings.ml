open Ppxlib
module GW = Gospel.Warnings

type level = Warning | Error
type kind = ..
type kind += GospelError of Gospel.Warnings.kind | Unsupported of string

exception Unkown_kind

type t = kind * Location.t

let level = function
  | GospelError _ -> Error
  | Unsupported _ -> Warning
  | _ -> raise Unkown_kind

exception Error of t

open Fmt

let pp_level ppf = function
  | Warning -> GW.styled_list [ `Magenta; `Bold ] string ppf "Warning"
  | Error -> GW.styled_list [ `Red; `Bold ] string ppf "Error"

let pp_kind ppf = function
  | GospelError k -> pf ppf "Gospel error: %a" Gospel.Warnings.pp_kind k
  | Unsupported msg -> pf ppf "Skipping clause:@ unsupported %s" msg
  | _ -> raise Unkown_kind

let pp_param pp_kind level ppf (k, loc) =
  let pp_sort ppf k = pp_level ppf (level k) in
  GW.pp_gen pp_sort pp_kind ppf loc k

let pp = pp_param pp_kind level

let () =
  Printexc.register_printer (function
    | Error t -> Some (Fmt.str "%a" pp t)
    | _ -> None)

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

type 'a reserr = ('a, W.t list) result * W.t list

let ok x = (Result.ok x, [])
let error e = (Result.error [ e ], [])
let warns ws = (Result.ok (), ws)
let warn w = warns [ w ]

let ( let* ) x f =
  match x with
  | Ok v, warns1 ->
      let res, warns2 = f v in
      (res, warns1 @ warns2)
  | (Error _, _) as x -> x

let ( and* ) (a, aw) (b, bw) =
  let r =
    match (a, b) with
    | Error e0, Error e1 -> Error (e0 @ e1)
    | Error e, _ | _, Error e -> Error e
    | Ok a, Ok b -> Ok (a, b)
  in
  (r, aw @ bw)

let pp_kind ppf kind =
  let open Fmt in
  match kind with
  | Constant_value id -> pf ppf "%a is a constant." W.quoted id
  | Returning_sut id -> pf ppf "%a returns a sut." W.quoted id
  | No_sut_argument id -> pf ppf "%a have no sut argument." W.quoted id
  | Multiple_sut_arguments id ->
      pf ppf "%a have multiple sut arguments." W.quoted id
  | _ -> W.pp_kind ppf kind

let pp_errors = W.pp_param pp_kind level |> Fmt.list
let pp pp_ok = Fmt.(pair (result ~ok:pp_ok ~error:pp_errors) pp_errors)

let promote r =
  let rec aux = function
    | [] -> ok []
    | ((Ok _, _) as x) :: xs ->
        let* y = x and* ys = aux xs in
        ok (y :: ys)
    | (Error errs, ws) :: xs ->
        let* _ = warns ws and* _ = auxes errs in
        aux xs
  and auxes = function
    | [] -> ok ()
    | ((k, _) as e) :: es -> (
        match level k with
        | W.Warning ->
            let* _ = warn e in
            auxes es
        | W.Error -> error e)
  in
  aux r

let map f l = List.map f l |> promote

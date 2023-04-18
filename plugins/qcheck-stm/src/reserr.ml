module W = Ortac_core.Warnings

type W.kind +=
  | Constant_value of string
  | Returning_sut of string
  | No_sut_argument of string
  | Multiple_sut_arguments of string
  | No_sut_type of string
  | No_init_function of string
  | Syntax_error_in_type of string
  | Syntax_error_in_init_sut of string
  | Sut_type_not_supported of string
  | Init_sut_not_supported of string
  | Type_parameter_not_instantiated of string
  | Type_not_supported_for_sut_parameter of string
  | Incompatible_type of string
  | Sut_type_not_specified of string
  | No_models of string

let level kind =
  match kind with
  | Constant_value _ | Returning_sut _ | No_sut_argument _
  | Multiple_sut_arguments _ | Incompatible_type _ ->
      W.Warning
  | No_sut_type _ | No_init_function _ | Syntax_error_in_type _
  | Sut_type_not_supported _ | Type_not_supported_for_sut_parameter _
  | Init_sut_not_supported _ | Syntax_error_in_init_sut _
  | Type_parameter_not_instantiated _ | Sut_type_not_specified _ | No_models _
    ->
      W.Error
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

let fmap f r =
  let* r = r in
  ok (f r)

let ( <$> ) = fmap

let pp_kind ppf kind =
  let open Fmt in
  match kind with
  | Constant_value id -> pf ppf "%a is a constant." W.quoted id
  | Returning_sut id -> pf ppf "%a returns a sut." W.quoted id
  | No_sut_argument id -> pf ppf "%a have no sut argument." W.quoted id
  | Multiple_sut_arguments id ->
      pf ppf "%a have multiple sut arguments." W.quoted id
  | No_sut_type ty ->
      pf ppf "Type %a is not declared in the module." W.quoted ty
  | No_init_function f ->
      pf ppf "Function %a is not declared in the module." W.quoted f
  | Syntax_error_in_type t ->
      pf ppf "%a is not a well formed type expression." W.quoted t
  | Syntax_error_in_init_sut s ->
      pf ppf "%a is not a well formed OCaml expression." W.quoted s
  | Sut_type_not_supported ty ->
      pf ppf "The type %a given for the system under test is not supported."
        W.quoted ty
  | Init_sut_not_supported e ->
      pf ppf "The expression %a given for init_sut in not supported." W.quoted e
  | Type_parameter_not_instantiated ty ->
      pf ppf "Type parameter %a should be instantiated." W.quoted ty
  | Type_not_supported_for_sut_parameter ty ->
      pf ppf
        "Type parameter in %a are not supported as type argument for the \
         system under test."
        W.quoted ty
  | Incompatible_type v ->
      pf ppf
        "Type of system under test in %a is incompatible with command line \
         argument."
        W.quoted v
  | Sut_type_not_specified ty ->
      pf ppf "The type %a given for the system under test is not specified."
        W.quoted ty
  | No_models ty ->
      pf ppf "The type %a given for the system under test has no models."
        W.quoted ty
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

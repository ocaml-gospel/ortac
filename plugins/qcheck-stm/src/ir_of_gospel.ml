open Gospel
open Tast
open Reserr
open Config

let lb_arg_is_of_type ts lb =
  match lb with
  | Lunit -> false
  | Lnone vs | Loptional vs | Lnamed vs | Lghost vs -> (
      match vs.vs_ty.ty_node with
      | Tyapp (ts', _) -> Ttypes.ts_equal ts ts'
      | _ -> false)

let lb_arg_is_not_of_type ty lb_arg = not (lb_arg_is_of_type ty lb_arg)

let constant_test vd =
  match vd.vd_args with
  | [] -> Value_is_a_constant (vd.vd_loc, vd.vd_name) |> error
  | _ -> ok vd

let argument_test ty vd =
  let p = lb_arg_is_of_type ty in
  let rec exactly_one = function
    | [] -> Value_have_no_sut_argument (vd.vd_loc, vd.vd_name) |> error
    | x :: xs when p x ->
        if List.exists p xs then
          Value_have_multiple_sut_arguments (vd.vd_loc, vd.vd_name) |> error
        else ok vd
    | _ :: xs -> exactly_one xs
  in
  exactly_one vd.vd_args

let return_test ty vd =
  if List.for_all (lb_arg_is_not_of_type ty) vd.vd_ret then ok vd
  else Value_return_sut (vd.vd_loc, vd.vd_name) |> error

let value_id config vd =
  let* _ = constant_test vd
  and* _ = argument_test config.sut vd
  and* _ = return_test config.sut vd in
  ok vd.vd_name

let val_desc config vd =
  let* id = value_id config vd in
  Ir.value id |> ok

let sig_item config s =
  match s.sig_desc with
  | Sig_val (vd, Nonghost) -> Some (val_desc config vd)
  | _ -> None

let signature config = List.filter_map (sig_item config)

let run path init sut =
  (* temporary implementation until Config uses the new Reserr *)
  match Config.init path init sut with
  | Ok (sigs, config) -> signature config sigs
  | Error _err -> assert false

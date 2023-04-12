open Gospel
open Tast
open Reserr

let constant_test vd =
  match vd.vd_args with
  | [] -> (Constant_value vd.vd_name.id_str, vd.vd_loc) |> error
  | _ -> ok ()

let val_desc vd =
  let* () = constant_test vd in
  Ir.value vd.vd_name |> ok

let sig_item _config s =
  match s.sig_desc with
  | Sig_val (vd, Nonghost) -> Some (val_desc vd)
  | _ -> None

let signature config = List.filter_map (sig_item config)

let run path init sut =
  (* temporary implementation until Config uses the new Reserr *)
  let open Reserr in
  let* sigs, config = Config.init path init sut in
  signature config sigs |> ok

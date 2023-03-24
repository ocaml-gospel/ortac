open Gospel
open Tast
open Ortac_core

type t = {
  context : Context.t;
  init : Identifier.Ident.t;
  sut : Ttypes.tysymbol;
}

let get_sut_ts_from_td sut td =
  let open Identifier.Ident in
  if td.td_ts.ts_ident.id_str = sut then Some td.td_ts else None

let get_sut_ts_from_sig_desc sut = function
  | Sig_type (_, tds, Nonghost) -> List.find_map (get_sut_ts_from_td sut) tds
  | _ -> None

let get_sut_ts_from_signature sut =
  let f signature_item = get_sut_ts_from_sig_desc sut signature_item.sig_desc in
  List.find_map f

let check_init_value_type sut vd =
  match (vd.vd_args, vd.vd_ret) with
  | [ Lunit ], [ Lnone vs ] -> (
      match vs.vs_ty.ty_node with
      | Tyapp (ts, _) ->
          if Ttypes.ts_equal ts sut then Some vd.vd_name else None
      | _ -> None)
  | _, _ -> None

let get_init_from_sig_desc sut init = function
  | Sig_val (vd, Nonghost) ->
      if vd.vd_name.id_str = init then check_init_value_type sut vd else None
  | _ -> None

let get_init_id_from_signature init sut =
  let f signature_item =
    get_init_from_sig_desc init sut signature_item.sig_desc
  in
  List.find_map f

let init path init sut =
  let module_name = Utils.module_name_of_path path in
  Parser_frontend.parse_ocaml_gospel path |> Utils.type_check [] path
  |> fun (env, sigs) ->
  assert (List.length env = 1);
  let namespace = List.hd env in
  let context = Context.init module_name namespace in
  match get_sut_ts_from_signature sut sigs with
  | Some sut -> (
      match get_init_id_from_signature sut init sigs with
      | Some init -> Ok (sigs, { context; init; sut })
      | None -> Error "Can't find init function")
  | None -> Error "Can't find sut"

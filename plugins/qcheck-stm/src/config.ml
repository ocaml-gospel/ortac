open Gospel
open Tast
open Ortac_core

type t = { context : Context.t; init : string; sut : Ttypes.tysymbol }

let get_sut_ts_from_td sut td =
  let open Identifier.Ident in
  if td.td_ts.ts_ident.id_str = sut then Some td.td_ts else None

let get_sut_ts_from_sig_desc sut = function
  | Sig_type (_, tds, Nonghost) -> List.find_map (get_sut_ts_from_td sut) tds
  | _ -> None

let get_sut_ts_from_signature sut =
  let f signature_item = get_sut_ts_from_sig_desc sut signature_item.sig_desc in
  List.find_map f

let init path init sut =
  let module_name = Utils.module_name_of_path path in
  Parser_frontend.parse_ocaml_gospel path |> Utils.type_check [] path
  |> fun (env, sigs) ->
  assert (List.length env = 1);
  let namespace = List.hd env in
  let context = Context.init module_name namespace in
  match get_sut_ts_from_signature sut sigs with
  | Some sut -> Ok { context; init; sut }
  | None -> Error "Can't find sut"

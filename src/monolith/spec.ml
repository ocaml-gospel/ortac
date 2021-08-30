open Ppxlib
module A = Ast_builder.Default
module B = Ortac_core.Builder

let loc = Location.none

let spec_definition (type_decl : Gospel.Tast.type_declaration) =
  let id = type_decl.td_ts.ts_ident.id_str in
  let gen = B.evar (Printf.sprintf "G.%s" id) in
  let printer = B.evar (Printf.sprintf "P.%s" id) in
  [%stri
    let [%p B.pvar id] =
      let neg = easily_constructible [%e gen] [%e printer] in
      let pos = deconstructible [%e printer] in
      ifpol neg pos]

let spec_option (sig_item : Gospel.Tast.signature_item) =
  match sig_item.sig_desc with
  | Gospel.Tast.Sig_type (_, [ type_decl ], _) ->
      Some (spec_definition type_decl)
  | _ -> None

let specs s =
  let name = B.noloc (Some "S") in
  let specs = List.filter_map spec_option s in
  let expr = A.pmod_structure ~loc specs in
  A.pstr_module ~loc (A.module_binding ~loc ~name ~expr)

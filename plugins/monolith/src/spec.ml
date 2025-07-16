open Ppxlib
open Gospel
module A = Ast_builder.Default
module B = Ortac_core.Builder

let loc = Location.none

let spec_abstract (type_decl : Tast.type_declaration) =
  let id = type_decl.td_ts.ts_ident.id_str in
  [%stri
    let [%p B.pvar id] = declare_abstract_type ~var:[%e A.estring ~loc id] ()]

let spec_constructor (type_decl : Tast.type_declaration) =
  let id = type_decl.td_ts.ts_ident.id_str in
  let gen = B.evar (Printf.sprintf "G.%s" id) in
  let printer = B.evar (Printf.sprintf "P.%s" id) in
  [%stri
    let [%p B.pvar id] =
      let neg = easily_constructible [%e gen] [%e printer] in
      let pos = deconstructible [%e printer] in
      ifpol neg pos]

let spec_dispatch (type_decl : Tast.type_declaration) =
  match type_decl.td_kind with
  | Pty_abstract -> Some (spec_abstract type_decl)
  | _ when type_decl.td_private = Tast.Private -> Some (spec_abstract type_decl)
  | Pty_variant _ | Pty_record _ -> Some (spec_constructor type_decl)

let spec_option (sig_item : Tast.signature_item) =
  match sig_item.sig_desc with
  | Tast.Sig_type (_, [ type_decl ], _) -> spec_dispatch type_decl
  | _ -> None

let specs s =
  let name = B.noloc (Some "S") in
  let specs = List.filter_map spec_option s in
  let expr = A.pmod_structure ~loc specs in
  A.pstr_module ~loc (A.module_binding ~loc ~name ~expr)

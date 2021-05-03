open Ppxlib
module A = Ast_builder.Default
module B = Ortac_core.Builder

let loc = Location.none

let str2printer ty =
  match ty with
  | "unit" -> [%expr PPrintOCaml.unit]
  | "bool" -> [%expr PPrintOCaml.bool]
  | "char" -> [%expr PPrintOCaml.char]
  | "int" -> [%expr PPrintOCaml.int]
  | "string" -> [%expr PPrintOCaml.string]
  | s -> failwith (Printf.sprintf "%s printer is not yet implementer" s)

let get_ty (ld : Gospel.Tterm.lsymbol Gospel.Tast.label_declaration) =
  match ld.ld_field.ls_value with
  | Some ty -> (
      match ty.ty_node with
      | Tyvar tvs -> tvs.tv_name.id_str
      | Tyapp (tys, _params) -> tys.ts_ident.id_str)
  | None -> failwith "can't find type"

let mk_field (ld : Gospel.Tterm.lsymbol Gospel.Tast.label_declaration) =
  let field = ld.ld_field.ls_name.id_str in
  let printer = str2printer (get_ty ld) in
  [%expr [%e B.estring field], [%e printer] [%e B.evar field]]

let record_printer (rec_decl : Gospel.Tast.rec_declaration) =
  let fields =
    List.map
      (fun (ld : Gospel.Tterm.lsymbol Gospel.Tast.label_declaration) ->
        ld.ld_field.ls_name.id_str)
      rec_decl.rd_ldl
  in
  let fields_pat = String.concat ";" fields in
  let fields_printer = List.map mk_field rec_decl.rd_ldl in
  let pat = B.pvar (Printf.sprintf "R.{ %s }" fields_pat) in
  let l = B.elist fields_printer in
  [%expr fun [%p pat] -> PPrintOCaml.record "" [%e l]]

let printer_expr (ty_kind : Gospel.Tast.type_kind) =
  match ty_kind with
  | Pty_abstract -> failwith "printer for abstract type not yet implemented"
  | Pty_variant _construtors ->
      failwith "printer for variant not yet implemented"
  | Pty_record rec_decl -> record_printer rec_decl
  | Pty_open -> failwith "printer for open not yet implemented"

let printer_definition (type_decl : Gospel.Tast.type_declaration) =
  let id = B.pvar type_decl.td_ts.ts_ident.id_str in
  let printer = printer_expr type_decl.td_kind in
  [%stri let [%p id] = [%e printer]]

let printer_option (sig_item : Gospel.Tast.signature_item) =
  match sig_item.sig_desc with
  | Gospel.Tast.Sig_type (_, [ type_decl ], _) ->
      Some (printer_definition type_decl)
  | _ -> None

let printers s =
  let name = B.noloc (Some "P") in
  let printers = List.filter_map printer_option s in
  let expr = A.pmod_structure ~loc printers in
  A.pstr_module ~loc (A.module_binding ~loc ~name ~expr)

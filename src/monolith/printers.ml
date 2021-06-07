open Ppxlib
open Gospel
module A = Ast_builder.Default
module B = Ortac_core.Builder

let loc = Location.none

let rec ty2printer (ty : Ttypes.ty) =
  match ty.ty_node with
  | Tyvar tvs -> tvs2printer tvs
  | Tyapp (tys, _tyl) -> tys2printer tys

and tvs2printer (tvs : Ttypes.tvsymbol) =
  match tvs.tv_name.id_str with
  | "unit" -> [%expr PPrintOCaml.unit]
  | "bool" -> [%expr PPrintOCaml.bool]
  | "char" -> [%expr PPrintOCaml.char]
  | "int" -> [%expr PPrintOCaml.int]
  | "string" -> [%expr PPrintOCaml.string]
  | s -> failwith (Printf.sprintf "%s printer is not yet implementer" s)

and tys2printer (tys : Ttypes.tysymbol) =
  match tys.ts_ident.id_str with
  | "unit" -> [%expr PPrintOCaml.unit]
  | "bool" -> [%expr PPrintOCaml.bool]
  | "char" -> [%expr PPrintOCaml.char]
  | "int" -> [%expr PPrintOCaml.int]
  | "string" -> [%expr PPrintOCaml.string]
  | s -> failwith (Printf.sprintf "%s printer is not yet implementer" s)

let lsymbol2printer (ls : Tterm.lsymbol) =
  match ls.ls_value with
  | Some ty -> ty2printer ty
  | None -> failwith "con't find type to build printer"

let mk_field (ld : Tterm.lsymbol Tast.label_declaration) =
  let field = ld.ld_field.ls_name.id_str in
  let printer = lsymbol2printer ld.ld_field in
  [%expr [%e B.estring field], [%e printer] [%e B.evar field]]

let record_printer (rec_decl : Tast.rec_declaration) =
  let fields =
    List.map
      (fun (ld : Tterm.lsymbol Tast.label_declaration) ->
        ld.ld_field.ls_name.id_str)
      rec_decl.rd_ldl
  in
  let fields_pat = String.concat ";" fields in
  let fields_printer = List.map mk_field rec_decl.rd_ldl in
  let pat = B.pvar (Printf.sprintf "R.{ %s }" fields_pat) in
  let l = B.elist fields_printer in
  [%expr fun [%p pat] -> PPrintOCaml.record "" [%e l]]

let printer_expr (ty_kind : Tast.type_kind) =
  match ty_kind with
  | Pty_abstract -> None
  | Pty_variant _construtors -> None
  | Pty_record rec_decl -> Some (record_printer rec_decl)
  | Pty_open -> None

let printer_definition (type_decl : Tast.type_declaration) =
  let id = B.pvar type_decl.td_ts.ts_ident.id_str in
  match printer_expr type_decl.td_kind with
  | None -> None
  | Some printer -> Some [%stri let [%p id] = [%e printer]]

let printer_option (sig_item : Tast.signature_item) =
  match sig_item.sig_desc with
  | Tast.Sig_type (_, [ type_decl ], _) -> printer_definition type_decl
  | _ -> None

let printers s =
  let name = B.noloc (Some "P") in
  let printers = List.filter_map printer_option s in
  let expr = A.pmod_structure ~loc printers in
  A.pstr_module ~loc (A.module_binding ~loc ~name ~expr)

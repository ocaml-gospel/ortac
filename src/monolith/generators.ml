open Ppxlib

let loc = Location.none

module A = Ast_builder.Default
module B = Ortac_core.Builder

let rec string2gen s params =
  match s with
  | "unit" -> [%expr Gen.unit]
  | "bool" -> [%expr Gen.bool]
  | "char" -> [%expr Gen.char]
  | "int" -> [%expr Gen.int 1024]
  | "string" -> [%expr Gen.string (Gen.int 1024) Gen.char]
  | "list" -> (
      match params with
      | [] -> failwith "list should have a parameter"
      | [ param ] ->
          [%expr
            Gen.list (Gen.int Int.max_int)
              [%e string2gen (Utils.ty_ident param) (Utils.ty_params param)]]
      | _ -> failwith "don't know what to do with more than one parameter")
  | "array" -> (
      match params with
      | [] -> failwith "array should have a parameter"
      | [ param ] ->
          [%expr
            Gen.array (Gen.int Int.max_int)
              [%e string2gen (Utils.ty_ident param) (Utils.ty_params param)]]
      | _ -> failwith "don't know what to do with more than one parameter")
  | s -> failwith (Printf.sprintf "%s is not yet implemented" s)

let record_generator (rec_decl : Gospel.Tast.rec_declaration) =
  let field (ld : Gospel.Tterm.lsymbol Gospel.Tast.label_declaration) =
    Printf.sprintf "R.%s" ld.ld_field.ls_name.id_str
  in
  let gen (ld : Gospel.Tterm.lsymbol Gospel.Tast.label_declaration) =
    let ty = Utils.get_ld_ident ld in
    let params = Utils.get_ty_params ld in
    [%expr [%e string2gen ty params] ()]
  in
  let r =
    A.pexp_record ~loc
      (List.map (fun ld -> (B.lident (field ld), gen ld)) rec_decl.rd_ldl)
      None
  in
  [%expr fun () -> [%e r]]

let generator_expr (ty_kind : Gospel.Tast.type_kind) =
  match ty_kind with
  | Pty_abstract -> failwith "generator for abstract type not yet implemented"
  | Pty_variant _constructors ->
      failwith "generator for variant not yet implemented"
  | Pty_record rec_decl -> record_generator rec_decl
  | Pty_open -> failwith "generator for open not yet implemented"

let generator_definition (type_decl : Gospel.Tast.type_declaration) =
  let id = B.pvar type_decl.td_ts.ts_ident.id_str in
  let generator = generator_expr type_decl.td_kind in
  [%stri let [%p id] = [%e generator]]

let generator_option (sig_item : Gospel.Tast.signature_item) =
  match sig_item.sig_desc with
  | Gospel.Tast.Sig_type (_, [ type_decl ], _) ->
      Some (generator_definition type_decl)
  | _ -> None

let generators s =
  let gen = List.filter_map generator_option s in
  let module_g = A.pmod_structure ~loc gen in
  let module_bind =
    A.module_binding ~loc ~name:(B.noloc (Some "G")) ~expr:module_g
  in
  A.pstr_module ~loc module_bind

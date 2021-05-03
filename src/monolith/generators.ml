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
      | [ param ] -> [%expr Gen.list (Gen.int Int.max_int) [%e ty2gen param]]
      | _ -> failwith "don't know what to do with more than one parameter")
  | "array" -> (
      match params with
      | [] -> failwith "array should have a parameter"
      | [ param ] -> [%expr Gen.array (Gen.int Int.max_int) [%e ty2gen param]]
      | _ -> failwith "don't know what to do with more than one parameter")
  | s -> failwith (Printf.sprintf "%s is not yet implemented" s)

and ty2gen (ty : Gospel.Ttypes.ty) =
  match ty.ty_node with
  | Tyvar tvs -> string2gen tvs.tv_name.id_str []
  | Tyapp (tys, params) -> string2gen tys.ts_ident.id_str params

let generator_nr _ = failwith "non-recursive generator not yet implemented"

let record_gen ty_name (rec_decl : Gospel.Tast.rec_declaration) =
  let id = B.pvar ty_name in
  let get_field (ld : Gospel.Tterm.lsymbol Gospel.Tast.label_declaration) =
    Printf.sprintf "R.%s" ld.ld_field.ls_name.id_str
  in
  let get_ty (ld : Gospel.Tterm.lsymbol Gospel.Tast.label_declaration) =
    match ld.ld_field.ls_value with
    | Some ty -> [%expr [%e ty2gen ty] ()]
    | None -> failwith "can't find type"
  in
  let r =
    A.pexp_record ~loc
      (List.map (fun x -> (B.lident (get_field x), get_ty x)) rec_decl.rd_ldl)
      None
  in
  [%stri let [%p id] = fun () -> [%e r]]

let generator_r (type_decl : Gospel.Tast.type_declaration) =
  let ty_name = type_decl.td_ts.ts_ident.id_str in
  match type_decl.td_kind with
  | Gospel.Tast.Pty_record rec_decl -> record_gen ty_name rec_decl
  | _ -> failwith "variant not yet implemented"

let generator rec_flag type_decl =
  match rec_flag with
  | Gospel.Tast.Nonrecursive -> generator_nr type_decl
  | Gospel.Tast.Recursive -> generator_r type_decl

let mk_generator (sig_item : Gospel.Tast.signature_item) =
  match sig_item.sig_desc with
  | Gospel.Tast.Sig_type (rec_flag, [ type_decl ], _) ->
      Some (generator rec_flag type_decl)
  | _ -> None

let mk_generators s =
  let gen = List.filter_map mk_generator s in
  let module_g = A.pmod_structure ~loc gen in
  let module_bind =
    A.module_binding ~loc ~name:(B.noloc (Some "G")) ~expr:module_g
  in
  A.pstr_module ~loc module_bind

open Ppxlib

module M : Ortac_core.Backend.S = struct
  let prelude =
    let loc = Location.none in
    [ [%stri open Ortac_runtime] ]
end

let loc = Location.none

module G = Ortac_core.Ortac.Make (M)
module A = Ast_builder.Default
module B = Ortac_core.Builder

let mk_reference rtac =
  let module_r = A.pmod_structure ~loc rtac in
  let module_bind =
    A.module_binding ~loc ~name:(B.noloc (Some "R")) ~expr:module_r
  in
  A.pstr_module ~loc module_bind

let mk_candidate module_name =
  let module_c = A.pmod_ident ~loc (B.lident module_name) in
  let module_bind =
    A.module_binding ~loc ~name:(B.noloc (Some "C")) ~expr:module_c
  in
  A.pstr_module ~loc module_bind

module Generators = struct
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
      ld.ld_field.ls_name.id_str
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
end

module Printers = struct end

open Generators

module Specs = struct end

let is_arrow = function Ptyp_arrow _ -> true | _ -> false

let find_gen s =
  match s.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "unit"; _ }, _) -> [%expr Gen.unit]
  | Ptyp_constr ({ txt = Lident "int"; _ }, _) -> [%expr Gen.int Int.max_int]
  | Ptyp_constr ({ txt = Lident "bool"; _ }, _) -> [%expr Gen.bool]
  | Ptyp_constr ({ txt = Lident "string"; _ }, _) ->
      [%expr Gen.string (Gen.int 1024) Gen.char]
  | _ -> failwith "gen not implemented yet"

let find_printer s =
  match s.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "unit"; _ }, _) -> [%expr Print.unit]
  | Ptyp_constr ({ txt = Lident "int"; _ }, _) -> [%expr Print.int]
  | Ptyp_constr ({ txt = Lident "bool"; _ }, _) -> [%expr Print.bool]
  | Ptyp_constr ({ txt = Lident "string"; _ }, _) -> [%expr Print.string]
  | _ -> failwith "printer not implemented yet"

let rec translate_ret s =
  match s.ptyp_desc with
  | Ptyp_var s -> B.evar s
  | Ptyp_constr ({ txt = Lident "unit"; _ }, _) -> [%expr unit]
  | Ptyp_constr ({ txt = Lident "int"; _ }, _) -> [%expr int]
  | Ptyp_constr ({ txt = Lident "bool"; _ }, _) -> [%expr bool]
  | Ptyp_constr ({ txt = Lident "list"; _ }, [ param ]) ->
      [%expr list [%e translate_ret param]]
  | Ptyp_constr ({ txt = Lident "array"; _ }, [ param ]) ->
      [%expr M.deconstructible_array [%e find_printer param]]
  | _ -> failwith "monolith deconstructible spec not implemented yet"

let rec translate s =
  match s.ptyp_desc with
  | Ptyp_var s -> B.evar s
  | Ptyp_constr ({ txt = Lident ty; _ }, params) -> translate_constr ty params
  | Ptyp_arrow (_, x, y) when is_arrow y.ptyp_desc ->
      [%expr [%e translate x] ^> [%e translate y]]
  | Ptyp_arrow (_, x, y) -> [%expr [%e translate x] ^!> [%e translate_ret y]]
  | _ -> failwith "monolith constructible spec not implemented yet"

and translate_constr ty params =
  match ty with
  | "unit" -> [%expr unit]
  | "bool" -> [%expr bool]
  | "char" -> [%expr char]
  | "int" -> [%expr M.constructible_int]
  | "string" -> [%expr M.string]
  | "list" -> (
      match params with
      | [] -> failwith "List should have a param"
      | [ param ] -> [%expr List [%e translate param]]
      | _ -> failwith "don't know what to do with List with multiple params")
  | "array" -> (
      match params with
      | [] -> failwith "Array should have a param"
      | [ param ] ->
          [%expr
            M.constructible_array [%e find_gen param] [%e find_printer param]]
      | _ -> failwith "don't know what to do with Array with multiple params")
  | t -> failwith (Printf.sprintf "%s is not yet implemented" t)

let mk_declaration (sig_item : Gospel.Tast.signature_item) =
  match sig_item.sig_desc with
  | Gospel.Tast.Sig_val (decl, _ghost) ->
      let fun_name = decl.vd_name.id_str in
      let fun_type = decl.vd_type in
      let msg = B.estring (Printf.sprintf "%s is Ok" fun_name) in
      let reference = Printf.sprintf "R.%s" fun_name in
      let candidate = Printf.sprintf "C.%s" fun_name in
      Some
        [%expr
          let spec = [%e translate fun_type] in
          declare [%e msg] spec [%e B.evar reference] [%e B.evar candidate]]
  | _ -> None

let mk_declarations s =
  match List.filter_map mk_declaration s with
  | [] -> raise (failwith "module is empty")
  | [ e ] -> [%stri let () = [%e e]]
  | e1 :: es -> [%stri let () = [%e List.fold_left B.pexp_sequence e1 es]]

let mk_specs s =
  let main =
    [%stri
      let () =
        let fuel = 10 in
        main fuel]
  in
  [ mk_declarations s; main ]

let standalone module_name s =
  let mod_ref = mk_reference (G.signature module_name s) in
  let mod_can = mk_candidate module_name in
  let mod_gen = mk_generators s in
  let specs = mk_specs s in
  [%stri open Monolith]
  ::
  [%stri module M = Monolith_runtime] :: mod_ref :: mod_can :: mod_gen :: specs

let generate path =
  let module_name = Ortac_core.Utils.module_name_of_path path in
  Gospel.Parser_frontend.parse_ocaml_gospel path
  |> Ortac_core.Utils.type_check [] path
  |> standalone module_name
  |> Ppxlib_ast.Pprintast.structure Fmt.stdout

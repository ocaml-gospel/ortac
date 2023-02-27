module W = Warnings
open Types
open Ppxlib
open Gospel
module T = Translation

module P = struct
type pack = { ir : Translated.t; context : Context.t }
let pack ir context = { ir; context }
let unpack pack = (pack.ir, pack.context)
end

let register_name = gen_symbol ~prefix:"__error"

let term_printer text global_loc (t : Tterm.term) =
  try
    String.sub text
      (t.t_loc.loc_start.pos_cnum - global_loc.loc_start.pos_cnum)
      (t.t_loc.loc_end.pos_cnum - t.t_loc.loc_start.pos_cnum)
  with Invalid_argument _ -> Fmt.str "%a" Tterm_printer.print_term t

let type_of_ty ~ir (ty : Ttypes.ty) =
  match ty.ty_node with
  | Tyvar a ->
      Translated.type_ ~name:a.tv_name.id_str ~loc:a.tv_name.id_loc
        ~mutable_:Translated.Unknown ~ghost:Tast.Nonghost
  | Tyapp (ts, _tvs) -> (
      match Translated.get_type ts ir with
      | None ->
          let mutable_ = Mutability.ty ~ir ty in
          Translated.type_ ~name:ts.ts_ident.id_str ~loc:ts.ts_ident.id_loc
            ~mutable_ ~ghost:Tast.Nonghost
      | Some type_ -> type_)

let vsname (vs : Symbols.vsymbol) = Fmt.str "%a" Tast.Ident.pp vs.vs_name

let var_of_vs ~ir (vs : Symbols.vsymbol) : Translated.ocaml_var =
  let name = vsname vs in
  let label = Nolabel in
  let type_ = type_of_ty ~ir vs.vs_ty in
  { name; label; type_; modified = false; consumed = false }

let var_of_arg ~ir arg : Translated.ocaml_var =
  let label, name =
    match arg with
    | Tast.Lunit -> (Nolabel, "()")
    | Tast.Lnone vs | Tast.Lghost vs -> (Nolabel, vsname vs)
    | Tast.Loptional vs ->
        let name = vsname vs in
        (Optional name, name)
    | Tast.Lnamed vs ->
        let name = vsname vs in
        (Labelled name, name)
  in
  let type_ = type_of_ty ~ir (Tast_helper.ty_of_lb_arg arg) in
  { name; label; type_; modified = false; consumed = false }

let type_ ~pack ~ghost (td : Tast.type_declaration) =
  let ir, context = P.unpack pack in
  let name = td.td_ts.ts_ident.id_str in
  let loc = td.td_loc in
  let mutable_ = Mutability.type_declaration ~ir td in
  let type_ = Translated.type_ ~name ~loc ~mutable_ ~ghost in
  let process ~type_ (spec : Tast.type_spec) =
    let term_printer = Fmt.str "%a" Tterm_printer.print_term in
    let mutable_ = Mutability.(max type_.Translated.mutable_ (type_spec ~ir spec)) in
    let type_ =
      type_
      |> T.with_models ~context spec.ty_fields
      |> T.with_invariants ~context ~term_printer spec.ty_invariants
    in
    { type_ with mutable_ }
  in
  let type_ = Option.fold ~none:type_ ~some:(process ~type_) td.td_spec in
  let type_item = Translated.Type type_ in
  let ir = ir |> Translated.add_translation type_item |> Translated.add_type td.td_ts type_ in
  P.pack ir context

let types ~pack ~ghost =
  List.fold_left (fun pack -> type_ ~pack ~ghost) pack

let value ~pack ~ghost (vd : Tast.val_description) =
  let ir, context = P.unpack pack in
  let name = vd.vd_name.id_str in
  let loc = vd.vd_loc in
  let register_name = register_name () in
  let arguments = List.map (var_of_arg ~ir) vd.vd_args in
  let returns = List.map (var_of_arg ~ir) vd.vd_ret in
  let pure = false in
  let value =
    Translated.value ~name ~loc ~register_name ~arguments ~returns ~pure ~ghost
  in
  let process ~value (spec : Tast.val_spec) =
    let term_printer = term_printer spec.sp_text spec.sp_loc in
    let value =
      value
      |> T.with_checks ~context ~term_printer spec.sp_checks
      |> T.with_pres ~context ~term_printer spec.sp_pre
      |> T.with_posts ~context ~term_printer spec.sp_post
      |> T.with_xposts ~context ~term_printer spec.sp_xpost
      |> T.with_consumes spec.sp_cs
      |> T.with_modified spec.sp_wr
    in
    { value with pure = spec.sp_pure }
  in
  let value = Option.fold ~none:value ~some:(process ~value) vd.vd_spec in
  let value_item = Translated.Value value in
  let context =
    if value.pure then
      let ls = Context.get_ls context [ name ] in
      Context.add_function ls name context
    else context
  in
  let ir = Translated.add_translation value_item ir in
  P.pack ir context

let constant ~pack ~ghost (vd : Tast.val_description) =
  let ir, context = P.unpack pack in
  let name = vd.vd_name.id_str in
  let loc = vd.vd_loc in
  let register_name = register_name () in
  let type_ =
    assert (List.length vd.vd_ret = 1);
    type_of_ty ~ir (Tast_helper.ty_of_lb_arg (List.hd vd.vd_ret))
  in
  let constant = Translated.constant ~name ~loc ~register_name ~type_ ~ghost in
  let process ~constant (spec : Tast.val_spec) =
    let term_printer = term_printer spec.sp_text spec.sp_loc in
    constant |> T.with_constant_checks ~context ~term_printer spec.sp_post
  in
  let c = Option.fold ~none:constant ~some:(process ~constant) vd.vd_spec in
  let ir = Translated.add_translation (Constant c) ir in
  P.pack ir context

let function_of (kind : [ `Function | `Predicate ]) ~pack (f : Tast.function_)
    =
  let ir, context = P.unpack pack in
  let name = gen_symbol ~prefix:("__logical_" ^ f.fun_ls.ls_name.id_str) () in
  let loc = f.fun_loc in
  let rec_ = f.fun_rec in
  let arguments = List.map (var_of_vs ~ir) f.fun_params in
  let definition =
    Option.map (T.function_definition ~context f.fun_ls name) f.fun_def
  in
  let translation =
    match kind with
    | `Function -> Translated.Function { name; loc; rec_; arguments; definition }
    | `Predicate -> Translated.Predicate { name; loc; rec_; arguments; definition }
  in
  let ir = Translated.add_translation translation ir in
  let context = Context.add_function f.fun_ls name context in
  P.pack ir context

let function_ = function_of `Function
let predicate = function_of `Predicate

let axiom ~pack (ax : Tast.axiom) =
  let ir, context = P.unpack pack in
  let name = ax.ax_name.id_str in
  let loc = ax.ax_loc in
  let register_name = register_name () in
  let definition = T.axiom_definition ~context ~register_name ax.ax_term in
  let ir = Translated.add_translation (Axiom { name; loc; register_name; definition }) ir in
  P.pack ir context

let signature ~context s =
  let pack = P.pack (Translated.init context) context in
  List.fold_left
    (fun pack (sig_item : Tast.signature_item) ->
      match sig_item.sig_desc with
      | Sig_val (vd, ghost) when vd.vd_args <> [] -> value ~pack ~ghost vd
      | Sig_val (vd, ghost) -> constant ~pack ~ghost vd
      | Sig_type (_rec, td, ghost) -> types ~pack ~ghost td
      | Sig_function func when Option.is_none func.fun_ls.ls_value ->
          predicate ~pack func
      | Sig_function func -> function_ ~pack func
      | Sig_axiom ax -> axiom ~pack ax
      | _ -> pack)
    pack s |> P.unpack |> fst


module W = Warnings
open Types
open Ppxlib
open Gospel
open Translated
module T = Translation

let register_name = gen_symbol ~prefix:"__error"

let term_printer text global_loc (t : Tterm.term) =
  try
    String.sub text
      (t.t_loc.loc_start.pos_cnum - global_loc.loc_start.pos_cnum)
      (t.t_loc.loc_end.pos_cnum - t.t_loc.loc_start.pos_cnum)
  with Invalid_argument _ -> Fmt.str "%a" Tterm_printer.print_term t

let type_of_ty ~context (ty : Ttypes.ty) =
  match ty.ty_node with
  | Tyvar a ->
      Translated.type_ ~name:a.tv_name.id_str ~loc:a.tv_name.id_loc
        ~mutable_:Translated.Unknown ~ghost:Tast.Nonghost
  | Tyapp (ts, _tvs) -> (
      match Context.get_type ts context with
      | None ->
          let mutable_ = Mutability.ty ~context ty in
          Translated.type_ ~name:ts.ts_ident.id_str ~loc:ts.ts_ident.id_loc
            ~mutable_ ~ghost:Tast.Nonghost
      | Some type_ -> type_)

let vsname (vs : Symbols.vsymbol) = Fmt.str "%a" Tast.Ident.pp vs.vs_name

let var_of_vs ~context (vs : Symbols.vsymbol) : Translated.ocaml_var =
  let name = vsname vs in
  let label = Nolabel in
  let type_ = type_of_ty ~context vs.vs_ty in
  { name; label; type_; modified = false; consumed = false }

let var_of_arg ~context arg : Translated.ocaml_var =
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
  let type_ = type_of_ty ~context (Tast_helper.ty_of_lb_arg arg) in
  { name; label; type_; modified = false; consumed = false }

let type_ ~context ~ghost (td : Tast.type_declaration) =
  let name = td.td_ts.ts_ident.id_str in
  let loc = td.td_loc in
  let mutable_ = Mutability.type_declaration ~context td in
  let type_ = type_ ~name ~loc ~mutable_ ~ghost in
  let process ~type_ (spec : Tast.type_spec) =
    let term_printer = Fmt.str "%a" Tterm_printer.print_term in
    let mutable_ = Mutability.(max type_.mutable_ (type_spec ~context spec)) in
    let type_ =
      type_
      |> T.with_models ~context spec.ty_fields
      |> T.with_invariants ~context ~term_printer spec.ty_invariants
    in
    { type_ with mutable_ }
  in
  let type_ = Option.fold ~none:type_ ~some:(process ~type_) td.td_spec in
  let type_item = Type type_ in
  context |> Context.add_translation type_item |> Context.add_type td.td_ts type_

let types ~context ~ghost =
  List.fold_left (fun context -> type_ ~context ~ghost) context

let value ~context ~ghost (vd : Tast.val_description) =
  let name = vd.vd_name.id_str in
  let loc = vd.vd_loc in
  let register_name = register_name () in
  let arguments = List.map (var_of_arg ~context) vd.vd_args in
  let returns = List.map (var_of_arg ~context) vd.vd_ret in
  let pure = false in
  let value =
    value ~name ~loc ~register_name ~arguments ~returns ~pure ~ghost
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
  let value_item = Value value in
  let context =
    if value.pure then
      let ls = Context.get_ls context [ name ] in
      Context.add_function ls name context
    else context
  in
  Context.add_translation value_item context

let constant ~context ~ghost (vd : Tast.val_description) =
  let name = vd.vd_name.id_str in
  let loc = vd.vd_loc in
  let register_name = register_name () in
  let type_ =
    assert (List.length vd.vd_ret = 1);
    type_of_ty ~context (Tast_helper.ty_of_lb_arg (List.hd vd.vd_ret))
  in
  let constant = constant ~name ~loc ~register_name ~type_ ~ghost in
  let process ~constant (spec : Tast.val_spec) =
    let term_printer = term_printer spec.sp_text spec.sp_loc in
    constant |> T.with_constant_checks ~context ~term_printer spec.sp_post
  in
  let c = Option.fold ~none:constant ~some:(process ~constant) vd.vd_spec in
  Context.add_translation (Constant c) context

let function_of (kind : [ `Function | `Predicate ]) ~context (f : Tast.function_)
    =
  let name = gen_symbol ~prefix:("__logical_" ^ f.fun_ls.ls_name.id_str) () in
  let loc = f.fun_loc in
  let rec_ = f.fun_rec in
  let arguments = List.map (var_of_vs ~context) f.fun_params in
  let definition =
    Option.map (T.function_definition ~context f.fun_ls name) f.fun_def
  in
  let translation =
    match kind with
    | `Function -> Function { name; loc; rec_; arguments; definition }
    | `Predicate -> Predicate { name; loc; rec_; arguments; definition }
  in
  context |> Context.add_translation translation |> Context.add_function f.fun_ls name

let function_ = function_of `Function
let predicate = function_of `Predicate

let axiom ~context (ax : Tast.axiom) =
  let name = ax.ax_name.id_str in
  let loc = ax.ax_loc in
  let register_name = register_name () in
  let definition = T.axiom_definition ~context ~register_name ax.ax_term in
  Context.add_translation (Axiom { name; loc; register_name; definition }) context

let signature ~context s =
  List.fold_left
    (fun context (sig_item : Tast.signature_item) ->
      match sig_item.sig_desc with
      | Sig_val (vd, ghost) when vd.vd_args <> [] -> value ~context ~ghost vd
      | Sig_val (vd, ghost) -> constant ~context ~ghost vd
      | Sig_type (_rec, td, ghost) -> types ~context ~ghost td
      | Sig_function func when Option.is_none func.fun_ls.ls_value ->
          predicate ~context func
      | Sig_function func -> function_ ~context func
      | Sig_axiom ax -> axiom ~context ax
      | _ -> context)
    context s

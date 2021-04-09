open Ppxlib
open Gospel
open Builder

let value (val_desc : Tast.val_description) =
  let process (spec : Tast.val_spec) =
    (* Declaration location *)
    let loc = val_desc.vd_loc in
    if List.length spec.sp_args = 0 then
      raise (Unsupported (Some loc, "non-function value"));
    (* Arguments *)
    let eargs, pargs = of_gospel_args spec.sp_args in
    (* Returned pattern *)
    let ret_pat, ret_expr = returned_pattern spec.sp_ret in
    let loc_name = gen_symbol ~prefix:"__loc" () in
    let let_loc next =
      [%expr
        let [%p pvar loc_name] = [%e elocation loc] in
        [%e next]]
    in
    let eloc = evar loc_name in
    let acc_name = gen_symbol ~prefix:"__acc" () in
    let post_checks = post acc_name val_desc.vd_name.id_str eloc spec.sp_post in
    let pre_checks = pre acc_name val_desc.vd_name.id_str eloc spec.sp_pre in
    let call = pexp_apply (evar val_desc.vd_name.id_str) eargs in
    let check_raises =
      xpost_guard acc_name loc val_desc.vd_name.id_str eloc spec.sp_xpost call
    in
    let let_call next =
      [%expr
        let [%p ret_pat] = [%e check_raises] in
        [%e next]]
    in
    let let_acc next =
      [%expr
        let [%p pvar acc_name] = Errors.empty () in
        [%e next]]
    in
    let rep_expr = [%expr Errors.check_and_report [%e evar acc_name]] in
    let body =
      efun pargs @@ let_acc @@ let_loc @@ pexp_sequence pre_checks
      @@ pexp_sequence rep_expr
           (let_call @@ pexp_sequence post_checks
           @@ pexp_sequence rep_expr ret_expr)
    in
    [%stri let [%p pvar val_desc.vd_name.id_str] = [%e body]]
  in
  Option.map process val_desc.vd_spec

let signature =
  List.filter_map (fun (sig_item : Tast.signature_item) ->
      match sig_item.sig_desc with
      | Sig_val (decl, _ghost) -> value decl
      | _ -> None)

let main module_name s =
  try
    let include_lib =
      pmod_ident (lident module_name) |> include_infos |> pstr_include
    in
    let declarations = signature s in
    mk_open :: include_lib :: declarations
  with
  | Unsupported (_loc, msg) ->
      let open Fmt in
      failwith "%a: unsupported %s" (styled `Red string) "Error" msg
  | e -> raise e

open Ppxlib
open Gospel
open Fmt

module Make (G : Backend.S) = struct
  open Builder.Make (G)

  let of_gospel_args args =
    let to_string x = str "%a" Tast.Ident.pp x.Tterm.vs_name in
    List.fold_right
      (fun arg (eargs, pargs) ->
        match arg with
        | Tast.Lunit -> ((Nolabel, eunit) :: eargs, (Nolabel, punit) :: pargs)
        | Tast.Lnone x ->
            let s = to_string x in
            ((Nolabel, evar s) :: eargs, (Nolabel, pvar s) :: pargs)
        | Tast.Loptional x ->
            let s = to_string x in
            ((Optional s, evar s) :: eargs, (Nolabel, pvar s) :: pargs)
        | Tast.Lnamed x ->
            let s = to_string x in
            ((Labelled s, evar s) :: eargs, (Labelled s, pvar s) :: pargs)
        | Tast.Lghost _ -> (eargs, pargs))
      args ([], [])

  let value (val_desc : Tast.val_description) =
    let process (spec : Tast.val_spec) =
      (* Declaration location *)
      let loc = val_desc.vd_loc in
      if List.length spec.sp_args = 0 then
        raise (Unsupported (Some loc, "non-function value"));
      let setup_expr, loc_name, acc_name = mk_setup loc in
      (* Arguments *)
      let eargs, pargs = of_gospel_args spec.sp_args in
      (* Returned pattern *)
      let ret_pat, ret_expr = returned_pattern spec.sp_ret in
      let eloc = evar loc_name in
      let pre_checks =
        mk_pre_checks acc_name val_desc.vd_name.id_str eloc spec.sp_pre
      in
      let let_call =
        mk_call acc_name ret_pat loc val_desc.vd_name.id_str eloc spec.sp_xpost
          eargs
      in
      let post_checks =
        mk_post_checks acc_name val_desc.vd_name.id_str eloc spec.sp_post
      in
      let body =
        efun pargs @@ setup_expr @@ pre_checks @@ let_call @@ post_checks
        @@ ret_expr
      in
      [%stri let [%p pvar val_desc.vd_name.id_str] = [%e body]]
    in
    Option.map process val_desc.vd_spec

  let signature =
    List.filter_map (fun (sig_item : Tast.signature_item) ->
        match sig_item.sig_desc with
        | Sig_val (decl, _ghost) -> value decl
        | _ -> None)
end

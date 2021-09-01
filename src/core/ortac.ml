open Ppxlib
open Gospel

module Make (B : Backend.S) = struct
  open Builder
  module T = Translation

  let of_gospel_args args =
    let to_string x =
      Fmt.str "%a" Gospel.Tast.Ident.pp x.Gospel.Tterm.vs_name
    in
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
      let term_printer (t : Tterm.term) =
        match t.t_loc with
        | None -> Fmt.str "%a" Tterm.print_term t
        | Some loc ->
            String.sub spec.sp_text loc.loc_start.pos_cnum
              (loc.loc_end.pos_cnum - loc.loc_start.pos_cnum)
      in
      (* Declaration location *)
      let loc = val_desc.vd_loc in
      if List.length spec.sp_args = 0 then
        raise (T.Unsupported (Some loc, "non-function value"));
      let setup_expr, register_name = T.mk_setup loc val_desc.vd_name.id_str in
      let register_name = evar register_name in
      (* Arguments *)
      let eargs, pargs = of_gospel_args spec.sp_args in
      (* Returned pattern *)
      let ret_pat, ret_expr = T.returned_pattern spec.sp_ret in
      let pre_checks =
        T.mk_pre_checks ~register_name ~term_printer spec.sp_pre
      in
      let let_call =
        T.mk_call ~register_name ~term_printer ret_pat loc
          val_desc.vd_name.id_str spec.sp_xpost eargs
      in
      let post_checks =
        T.mk_post_checks ~register_name ~term_printer spec.sp_post
      in
      let body =
        efun pargs @@ setup_expr @@ pre_checks @@ let_call @@ post_checks
        @@ ret_expr
      in
      [%stri let [%p pvar val_desc.vd_name.id_str] = [%e body]]
    in
    Option.map process val_desc.vd_spec

  let signature module_name s =
    let declarations =
      List.filter_map
        (fun (sig_item : Tast.signature_item) ->
          match sig_item.sig_desc with
          | Sig_val (decl, _ghost) -> value decl
          | _ -> None)
        s
    in
    let include_lib =
      pmod_ident (lident module_name) |> include_infos |> pstr_include
    in
    B.prelude @ (include_lib :: declarations)
end

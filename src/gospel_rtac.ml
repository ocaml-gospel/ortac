open Ppxlib
open Gospel
open Fmt

module type G = sig
  val open_modules : structure_item list

  val report_pre : label -> expression

  val report_post : label -> expression

  val report_declared_exn : label -> label -> expression

  val report_undeclared_exn : expression -> label -> label -> expression
end

module Make (G : G) = struct
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

let choose = function
  | "default" -> (module Gen_original : Builder.G)
  | "monolith" -> (module Gen_monolith : Builder.G)
  | _ -> raise (failwith "not yet implemented")

let main generator module_name s =
  let module G = (val choose generator) in
  let module B = Builder.Make (G) in
  let open Make (G) in
  try
    let include_lib =
      B.pmod_ident (B.lident module_name) |> B.include_infos |> B.pstr_include
    in
    let declarations = signature s in
    B.mk_open @ include_lib :: declarations
  with
  | B.Unsupported (_loc, msg) ->
      let open Fmt in
      failwith "%a: unsupported %s" (styled `Red string) "Error" msg
  | e -> raise e

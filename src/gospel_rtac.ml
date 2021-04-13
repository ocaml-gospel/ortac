open Ppxlib
open Gospel
open Fmt

module type B = sig
  include Ast_builder.S

  val efun : (arg_label * pattern) list -> expression -> expression

  val returned_pattern : Tast.lb_arg list -> pattern * expression

  exception Unsupported of Location.t option * string

  val mk_setup : location -> (expression -> expression) * label * label

  val mk_pre_checks :
    label -> label -> expression -> Tterm.term list -> expression -> expression

  val mk_call :
    label ->
    pattern ->
    location ->
    label ->
    expression ->
    (Ttypes.xsymbol * (Tterm.pattern * Tterm.term) list) list ->
    (arg_label * expression) list ->
    expression ->
    expression

  val mk_post_checks :
    label -> label -> expression -> Tterm.term list -> expression -> expression
end

module Make_Gospel_rtac (B : B) = struct
  open B

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

let main opt module_name s =
  let module M = (val choose opt) in
  let module B = Builder.Make_Builder (M) in
  let open B in
  let open Make_Gospel_rtac (B) in
  try
    let include_lib =
      pmod_ident (lident module_name) |> include_infos |> pstr_include
    in
    let declarations = signature s in
    mk_open @ include_lib :: declarations
  with
  | Unsupported (_loc, msg) ->
      let open Fmt in
      failwith "%a: unsupported %s" (styled `Red string) "Error" msg
  | e -> raise e

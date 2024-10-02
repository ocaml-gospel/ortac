module Cfg = Config
open Ir
open Ppxlib
open Ortac_core.Builder
module Ident = Gospel.Identifier.Ident

let ty_default = Ptyp_constr (noloc (Lident "char"), [])
let pat_default = ppat_construct (lident "Char") None
let exp_default = evar "char"
let res_default = Ident.create ~loc:Location.none "res"
let list_append = list_fold_expr (qualify [ "Ortac_runtime" ] "append") "None"
let res = lident "Res"

let eexpected_value case e =
  pexp_construct (noloc (Ldot (Lident "Ortac_runtime", case))) (Some e)

let evalue = eexpected_value "Value"
let eprotected = eexpected_value "Protected_value"
let eexception = eexpected_value "Exception"

let eprotect call =
  let lazy_call = efun [ (Nolabel, punit) ] call in
  pexp_apply (evar "protect") [ (Nolabel, lazy_call); (Nolabel, eunit) ]

let may_raise_exception v =
  match (v.postcond.exceptional, v.postcond.checks) with
  | [], [] -> false
  | _, _ -> true

let subst_core_type inst ty =
  let rec aux ty =
    {
      ty with
      ptyp_desc =
        (match ty.ptyp_desc with
        | Ptyp_any -> ty_default
        | Ptyp_var x ->
            Option.fold ~none:ty_default
              ~some:(fun x -> x.ptyp_desc)
              (List.assoc_opt x inst)
        | Ptyp_arrow (x, l, r) ->
            let l = aux l and r = aux r in
            Ptyp_arrow (x, l, r)
        | Ptyp_tuple elems ->
            let elems = List.map aux elems in
            Ptyp_tuple elems
        | Ptyp_constr (c, args) ->
            let args = List.map aux args in
            Ptyp_constr (c, args)
        | Ptyp_object (_, _)
        | Ptyp_class (_, _)
        | Ptyp_alias (_, _)
        | Ptyp_variant (_, _, _)
        | Ptyp_poly (_, _)
        | Ptyp_package _ | Ptyp_extension _ ->
            failwith "Case should not happen in `subst_core_type'");
    }
  in
  aux ty

let lazy_force =
  let open Gospel in
  let open Tterm_helper in
  let vs_name = Ident.create ~loc:Location.none "Lazy.force"
  and vs_ty = Ttypes.fresh_ty_var "a" in
  let lazy_force = mk_term (Tvar { vs_name; vs_ty }) None Location.none in
  fun t ->
    Tterm_helper.(
      mk_term (Tapp (Symbols.fs_apply, [ lazy_force; t ])) None Location.none)

let ocaml_of_term cfg t =
  let open Ortac_core.Ocaml_of_gospel in
  let open Reserr in
  try term_with_catch ~context:cfg.Cfg.context t |> ok
  with W.Error e -> error e

(** [subst_term state ~gos_t ?old_lz ~old_t ?new_lz ~new_t trm] will substitute
    occurrences in [gos_t] with the associcated values from [new_t] or [old_t]
    depending on whether the occurrence appears above or under the [old]
    operator, adding a [Lazy.force] if the corresponding [xxx_lz] is [true]
    (defaults to [false]). [gos_t] must always be in a position in which it is
    applied to one of its model fields. Calling [subst_term] with [new_t] and
    [old_t] as the empty list will check that the term does not contain [gos_t] *)
let subst_term state ?(out_of_scope = []) ~gos_t ?(old_lz = false) ~old_t
    ?(new_lz = false) ~new_t term =
  let exception
    ImpossibleSubst of
      (Gospel.Tterm.term * [ `Never | `New | `Old | `NotModel | `OutOfScope ])
  in
  let rec aux cur_lz cur_t term =
    let open Gospel.Tterm in
    let next = aux cur_lz cur_t in
    match term.t_node with
    (* First: the only case where substitution happens, ie x.model *)
    | Tfield (({ t_node = Tvar { vs_name; vs_ty }; _ } as subt), ls)
      when List.mem vs_name gos_t ->
        if List.exists (fun (m, _) -> Ident.equal m ls.ls_name) state then
          match List.assoc_opt vs_name cur_t with
          | Some cur_t ->
              let t = { subt with t_node = Tvar { vs_name = cur_t; vs_ty } } in
              let t = if cur_lz then lazy_force t else t in
              { term with t_node = Tfield (t, ls) }
          | None ->
              raise
                (ImpossibleSubst
                   ( subt,
                     match (new_t, old_t) with
                     | [], [] -> `Never
                     | [], _ -> `New
                     | _, _ -> `Old ))
        else
          (* case x.f where f is _not_ a model field *)
          raise (ImpossibleSubst (term, `NotModel))
    (* If the first case didn't match, it must be because [gos_t] is not used to
       access one of its model fields, so we error out *)
    | Tvar { vs_name; _ } when List.mem vs_name gos_t ->
        raise (ImpossibleSubst (term, `NotModel))
    (* Then, we check if the variable is not out_of_scope in the function we are building *)
    | Tvar { vs_name; _ } when List.exists (Ident.equal vs_name) out_of_scope ->
        raise (ImpossibleSubst (term, `OutOfScope))
    | Tconst _ -> term
    | Tvar _ -> term
    | Tapp (ls, terms) -> { term with t_node = Tapp (ls, List.map next terms) }
    | Tfield (t, ls) -> { term with t_node = Tfield (next t, ls) }
    | Tif (cnd, thn, els) ->
        { term with t_node = Tif (next cnd, next thn, next els) }
    | Tlet (vs, t1, t2) -> { term with t_node = Tlet (vs, next t1, next t2) }
    | Tcase (t, brchs) ->
        {
          term with
          t_node =
            Tcase
              ( next t,
                List.map
                  (fun (p, ot, t) -> (p, Option.map next ot, next t))
                  brchs );
        }
    | Tquant (q, vs, t) -> { term with t_node = Tquant (q, vs, next t) }
    | Tlambda (ps, t) -> { term with t_node = Tlambda (ps, next t) }
    | Tbinop (o, l, r) -> { term with t_node = Tbinop (o, next l, next r) }
    | Tnot t -> { term with t_node = Tnot (next t) }
    | Told t -> aux old_lz old_t t
    | Ttrue -> term
    | Tfalse -> term
  in
  let open Reserr in
  try ok (aux new_lz new_t term)
  with ImpossibleSubst (t, b) ->
    error (Impossible_term_substitution b, t.t_loc)

let translate_checks config state value sut_map t =
  let open Reserr in
  subst_term state ~gos_t:value.sut_vars ~old_t:sut_map ~new_t:sut_map t.term
  >>= ocaml_of_term config

let str_of_ident = Fmt.str "%a" Ident.pp
let longident_loc_of_ident id = str_of_ident id |> lident

let mk_cmd_pattern value =
  let pat_args = function
    | _, None -> punit
    | _, Some x -> ppat_var (noloc (str_of_ident x))
  in
  let args =
    match value.args with
    | [] -> None
    | [ x ] -> Some (pat_args x)
    | xs -> List.map pat_args xs |> ppat_tuple |> Option.some
  in
  let name = String.capitalize_ascii (str_of_ident value.id) |> lident in
  ppat_construct name args

let munge_longident cap ty lid =
  let open Reserr in
  match lid.txt with
  | Lident i | Ldot (Lident i, "t") | Ldot (Ldot (_, i), "t") | Ldot (_, i) ->
      let f =
        if cap then String.capitalize_ascii else String.uncapitalize_ascii
      in
      ok (f i)
  | Lapply (_, _) ->
      error
        (Type_not_supported (Fmt.str "%a" Pprintast.core_type ty), ty.ptyp_loc)

let pat_of_core_type inst typ =
  let rec aux ty =
    let open Reserr in
    match ty.ptyp_desc with
    | Ptyp_any -> ok pat_default
    | Ptyp_var v -> (
        match List.assoc_opt v inst with
        | None -> ok pat_default
        | Some t -> aux t)
    | Ptyp_constr (c, xs) ->
        let constr_str = lident <$> munge_longident true ty c
        and pat_arg =
          match xs with
          | [] -> ok None
          | xs -> (fun xs -> Some (ppat_tuple xs)) <$> promote_map aux xs
        in
        ppat_construct <$> constr_str <*> pat_arg
    | Ptyp_tuple xs ->
        let* pat_arg = ppat_tuple <$> promote_map aux xs in
        ppat_construct
          (lident ("Tup" ^ string_of_int (List.length xs)))
          (Some pat_arg)
        |> ok
    | _ ->
        error
          ( Type_not_supported (Fmt.str "%a" Pprintast.core_type typ),
            typ.ptyp_loc )
  in
  aux typ

let exp_of_core_type ?(use_small = false) inst typ =
  let rec aux ty =
    let open Reserr in
    match ty.ptyp_desc with
    | Ptyp_any -> ok exp_default
    | Ptyp_var v -> (
        match List.assoc_opt v inst with
        | None -> ok exp_default
        | Some t -> aux t)
    | Ptyp_constr (c, xs) -> (
        let* constr_id = munge_longident false ty c in
        let constr_str = evar constr_id in
        match xs with
        | [] ->
            if constr_id = "int" && use_small then evar "small_signed_int" |> ok
            else constr_str |> ok
        | xs ->
            pexp_apply constr_str
            <$> (List.map (fun e -> (Nolabel, e)) <$> promote_map aux xs))
    | Ptyp_tuple xs ->
        let tup_constr =
          pexp_ident (lident ("tup" ^ string_of_int (List.length xs)))
        in
        pexp_apply tup_constr
        <$> (List.map (fun e -> (Nolabel, e)) <$> promote_map aux xs)
    | _ ->
        error
          ( Type_not_supported (Fmt.str "%a" Pprintast.core_type typ),
            typ.ptyp_loc )
  in
  aux typ

let exp_of_ident id = pexp_ident (lident (str_of_ident id))

let arb_cmd_case config value =
  let open Reserr in
  let is_create = value.sut_vars = [] && Cfg.does_return_sut config value.ty in
  let epure = pexp_ident (lident "pure") in
  let pure e = pexp_apply epure [ (Nolabel, e) ] in
  let fun_cstr =
    let args =
      List.map
        (function
          | _, None -> (Nolabel, punit)
          | _, Some id -> (Nolabel, ppat_var (noloc (str_of_ident id))))
        value.args
    in
    let name = String.capitalize_ascii (str_of_ident value.id) |> lident in
    let body =
      pexp_construct name
        (pexp_tuple_opt
           (List.map
              (function
                | _, None -> eunit | _, Some id -> evar (str_of_ident id))
              value.args))
    in
    efun args body |> pure
  in
  let gen_args =
    (* XXX TODO: use `requires` clauses to build smarter generators *)
    traverse
      (fun (ty, _) -> exp_of_core_type ~use_small:is_create value.inst ty)
      value.args
  in
  let app l r = pexp_apply (evar "( <*> )") [ (Nolabel, l); (Nolabel, r) ] in
  List.fold_left app fun_cstr <$> gen_args

let arb_cmd config ir =
  let open Reserr in
  let* cmds = elist <$> promote_map (arb_cmd_case config) ir.values in
  let open Ppxlib in
  let let_open str e =
    pexp_open Ast_helper.(Opn.mk (Mod.ident (lident str |> noloc))) e
  in
  let oneof = let_open "Gen" (pexp_apply (evar "oneof") [ (Nolabel, cmds) ]) in
  let body =
    let_open "QCheck"
      (pexp_apply (evar "make")
         [ (Labelled "print", evar "show_cmd"); (Nolabel, oneof) ])
  in
  let pat = pvar "arb_cmd" in
  let expr = efun [ (Nolabel, ppat_any (* for now we don't use it *)) ] body in
  pstr_value Nonrecursive [ value_binding ~pat ~expr ] |> ok

let run_case config sut_name value =
  let lhs = mk_cmd_pattern value in
  let open Reserr in
  let* rhs =
    let* ty_show =
      let ret_ty = Ir.get_return_type value in
      if Cfg.is_sut config ret_ty then evar "sut" |> ok
      else exp_of_core_type value.inst ret_ty
    in
    let ty_show =
      if may_raise_exception value then
        pexp_apply (evar "result") [ (Nolabel, ty_show); (Nolabel, evar "exn") ]
      else ty_show
    in
    let suts, call =
      let efun = exp_of_ident value.id in
      let mk_arg = Option.fold ~none:eunit ~some:exp_of_ident in
      let trans_lb = function Optional l -> Labelled l | l -> l in
      let rec aux ty args sut_vars =
        match (ty.ptyp_desc, args, sut_vars) with
        | Ptyp_arrow (lb, l, r), xs, sut :: rest when Cfg.is_sut config l ->
            let tmp = gen_symbol ~prefix:(str_of_ident sut) () in
            let suts, args = aux r xs rest in
            (tmp :: suts, (trans_lb lb, evar tmp) :: args)
        | Ptyp_arrow (lb, _, r), x :: xs, suts ->
            let suts, args = aux r xs suts in
            (suts, (trans_lb lb, mk_arg x) :: args)
        | _, [], _ -> ([], [])
        | _, _, _ ->
            failwith
              "shouldn't happen (list of arguments should be consistent with \
               type)"
      in
      let suts, args = aux value.ty (List.map snd value.args) value.sut_vars in
      (suts, pexp_apply efun args)
    in
    let call = if may_raise_exception value then eprotect call else call in
    let call_res = gen_symbol ~prefix:"res" () in
    let pops body =
      List.fold_right
        (fun sut acc ->
          let expr = eapply (qualify [ "SUT" ] "pop") [ evar sut_name ] in
          let vb = value_binding ~pat:(pvar sut) ~expr in
          pexp_let Nonrecursive [ vb ] acc)
        suts body
    in
    let pushes =
      List.map
        (fun sut ->
          eapply (qualify [ "SUT" ] "push") [ evar sut_name; evar sut ])
        (List.rev suts)
      @
      (* If a sut is returned, push it last *)
      if Cfg.does_return_sut config value.ty then
        (* We can only push a sut if there was no exception *)
        if may_raise_exception value then
          [
            [%expr
              match [%e evar call_res] with
              | Ok res -> SUT.push [%e evar sut_name] res
              | Error _ -> ()];
          ]
        else [ [%expr SUT.push [%e evar sut_name] [%e evar call_res]] ]
      else []
    in
    let tail = List.fold_right pexp_sequence pushes (evar call_res) in
    let wrapped_call =
      pexp_let Nonrecursive
        [ value_binding ~pat:(pvar call_res) ~expr:call ]
        tail
      |> pops
    in
    let args = Some (pexp_tuple [ ty_show; wrapped_call ]) in
    pexp_construct res args |> ok
  in
  case ~lhs ~guard:None ~rhs |> ok

let run config ir =
  let cmd_name = gen_symbol ~prefix:"cmd" () in
  let sut_name = gen_symbol ~prefix:"sut" () in
  let open Reserr in
  let* cases = promote_map (run_case config sut_name) ir.values in
  let body = pexp_match (evar cmd_name) cases in
  let pat = pvar "run" in
  let expr = efun [ (Nolabel, pvar cmd_name); (Nolabel, pvar sut_name) ] body in
  pstr_value Nonrecursive [ value_binding ~pat ~expr ] |> ok

let pop_states state_ident value =
  List.mapi
    (fun idx sut ->
      let tmp_var = gen_symbol ~prefix:(str_of_ident sut) () in
      let tmp_id = Ident.create ~loc:Location.none tmp_var in
      let tmp_pat = pvar tmp_var in
      let tmp_expr =
        eapply
          (qualify [ "Model" ] "get")
          [ evar (str_of_ident state_ident); eint idx ]
      in
      (value_binding ~pat:tmp_pat ~expr:tmp_expr, (sut, tmp_id)))
    value.sut_vars
  |> List.split

let next_state_case state config state_ident nb_models value =
  let state_var = str_of_ident state_ident |> evar in
  let lhs = mk_cmd_pattern value in
  let open Reserr in
  let* idx, rhs =
    if value.sut_vars = [] && not (Cfg.does_return_sut config value.ty) then
      ([], state_ident |> str_of_ident |> evar) |> ok
    else
      let vbs, sut_map = pop_states state_ident value in
      let sut_map =
        if Cfg.does_return_sut config value.ty then
          let ret_id = List.hd value.ret in
          let ret_var = gen_symbol ~prefix:(str_of_ident ret_id) () in
          (ret_id, Ident.create ~loc:Location.none ret_var) :: sut_map
        else sut_map
      in
      let wrap e = if vbs <> [] then pexp_let Nonrecursive vbs e else e in
      (* Figure out which suts are used but not modified *)
      let modified_suts = List.map fst value.next_states in
      let modified_sut_map =
        List.filter (fun (id, _) -> List.mem id modified_suts) sut_map
      in
      let* next_states =
        promote_map
          (fun (sut, next_state) ->
            (* substitute state variable when under `old` operator and translate description into ocaml *)
            let descriptions =
              List.filter_map
                (fun (i, { model; description }) ->
                  subst_term ~out_of_scope:value.ret state ~gos_t:value.sut_vars
                    ~old_t:sut_map ~new_t:modified_sut_map description
                  >>= ocaml_of_term config
                  |> to_option
                  |> Option.map (fun description -> (i, model, description)))
                next_state.formulae
            in
            (* choose one and only one description per modified model *)
            let pick id =
              List.find_opt (fun (_, m, _) -> Ident.equal id m) descriptions
            in
            let* descriptions =
              promote_map
                (fun (id, loc) ->
                  of_option
                    ~default:
                      ( Ensures_not_found_for_next_state
                          (value.id.id_str, id.Ident.id_str),
                        loc )
                    (pick id))
                next_state.modifies
            in
            (* [idx], like [descriptions], is in the order of the modifies clauses *)
            let idx = List.map (fun (i, _, _) -> i) descriptions in
            match
              List.map
                (fun (_, m, e) -> (lident (str_of_ident m), e))
                descriptions
            with
            | [] -> ok (idx, sut, List.assoc sut sut_map |> str_of_ident |> evar)
            | fields ->
                ok
                  ( idx,
                    sut,
                    pexp_open
                      Ast_helper.(Opn.mk (Mod.ident (lident "ModelElt")))
                      (pexp_record fields
                         (if List.length fields = nb_models then None
                          else
                            Some (evar (List.assoc sut sut_map |> str_of_ident))))
                  ))
          value.next_states
      in
      let vbs_next_states, next_vars_assoc =
        let aux (_, sut, expr) (vbs, vars) =
          let next_state_var = gen_symbol ~prefix:(str_of_ident sut) () in
          let pat = pvar next_state_var in
          (value_binding ~pat ~expr :: vbs, (sut, next_state_var) :: vars)
        in
        List.fold_right aux next_states ([], [])
      in
      (* Get the next_state_vars in the correct, reversed order *)
      let next_state_vars =
        List.(rev_map (fun n -> assoc n next_vars_assoc) value.sut_vars)
      in
      let push_expr =
        List.fold_left
          (fun body next_var ->
            eapply (qualify [ "Model" ] "push") [ body; evar next_var ])
          (eapply
             (qualify [ "Model" ] "drop_n")
             [
               evar (str_of_ident state_ident);
               pexp_constant
                 (Pconst_integer
                    (List.length value.sut_vars |> string_of_int, None));
             ])
          next_state_vars
      in
      let push_expr =
        if Cfg.does_return_sut config value.ty then
          let ret_id = List.hd value.ret in
          let ret_var = List.assoc ret_id next_vars_assoc in
          eapply (qualify [ "Model" ] "push") [ push_expr; evar ret_var ]
        else push_expr
      in
      let new_state = pexp_let Nonrecursive vbs_next_states push_expr in
      let idx = List.fold_left (fun acc (i, _, _) -> i @ acc) [] next_states in
      let translate_checks = translate_checks config state value sut_map in
      let* checks = promote_map translate_checks value.postcond.checks in
      match checks with
      | [] -> ok (idx, wrap new_state)
      | _ ->
          ok
            ( idx,
              wrap
                (pexp_ifthenelse (list_and checks) new_state (Some state_var))
            )
  in
  (idx, case ~lhs ~guard:None ~rhs) |> ok

let next_state config ir =
  let cmd_name = gen_symbol ~prefix:"cmd" () in
  let state_name = gen_symbol ~prefix:"state" () in
  let state_ident = Ident.create ~loc:Location.none state_name in
  let nb_models = List.length ir.state in
  let open Reserr in
  let* idx_cases =
    promote_map
      (fun v ->
        let* i, c = next_state_case ir.state config state_ident nb_models v in
        ok ((v.id, i), c))
      ir.values
  in
  let idx, cases = List.split idx_cases in
  let body = pexp_match (evar cmd_name) cases in
  let pat = pvar "next_state" in
  let expr =
    efun [ (Nolabel, pvar cmd_name); (Nolabel, pvar state_name) ] body
  in
  (idx, pstr_value Nonrecursive [ value_binding ~pat ~expr ]) |> ok

let precond_case config state state_ident value =
  let lhs = mk_cmd_pattern value in
  let open Reserr in
  let* rhs =
    let vbs, sut_map = pop_states state_ident value in
    let wrap e = (if vbs <> [] then pexp_let Nonrecursive vbs e else e) |> ok in
    list_and
    <$> promote_map
          (fun t ->
            subst_term state ~gos_t:value.sut_vars ~old_t:[] ~new_t:sut_map t
            >>= ocaml_of_term config)
          value.precond
    >>= wrap
  in
  ok (case ~lhs ~guard:None ~rhs)

let precond config ir =
  let cmd_name = gen_symbol ~prefix:"cmd" () in
  let state_name = gen_symbol ~prefix:"state" () in
  let state_ident = Ident.create ~loc:Location.none state_name in
  let open Reserr in
  let* cases =
    promote_map (precond_case config ir.state state_ident) ir.values
  in
  let body = pexp_match (evar cmd_name) cases in
  let pat = pvar "precond" in
  let expr =
    efun [ (Nolabel, pvar cmd_name); (Nolabel, pvar state_name) ] body
  in
  pstr_value Nonrecursive [ value_binding ~pat ~expr ] |> ok

let expected_returned_value translate_postcond value =
  let open Reserr in
  let ( >>= ) = Option.bind in
  let ty_ret = Ir.get_return_type value in
  let ty_show = to_option @@ exp_of_core_type value.inst ty_ret in
  let ret_res ts val_ =
    match ts with
    | Some ty_show ->
        let args = pexp_tuple_opt [ ty_show; val_ ] in
        Some (pexp_construct res args)
    | None -> None
  in
  let ty_show_integer = evar "integer" in
  match (ty_ret.ptyp_desc, value.ret_values) with
  | Ptyp_constr ({ txt = Lident "unit"; _ }, _), _ -> ret_res ty_show eunit
  | Ptyp_constr ({ txt = Lident "int"; _ }, _), [ (t :: _ as xs) ]
    when t.term.t_ty = Some Gospel.Ttypes.ty_integer ->
      promote_map translate_postcond xs
      |> to_option
      >>= Fun.flip List.nth_opt 0
      >>= ret_res (Some ty_show_integer)
  | _, [ xs ] ->
      promote_map translate_postcond xs
      |> to_option
      >>= Fun.flip List.nth_opt 0
      >>= ret_res ty_show
  (* type of the returned value will be checked later with a proper error *)
  | _, _ -> None

let postcond_case config state invariants idx state_ident new_state_ident value
    =
  let open Reserr in
  let translate_postcond t =
    let vbs, sut_map_old, sut_map_new =
      let aux (acc_vbs, acc_old, acc_new) sut_var =
        let id = sut_var.Ident.id_str in
        let var_old = gen_symbol ~prefix:(id ^ "_old") () in
        let var_new = gen_symbol ~prefix:(id ^ "_new") () in
        let pat_old = pvar var_old in
        let pat_new = pvar var_new in
        let idx = List.length acc_old in
        let expr_old =
          [%expr Model.get [%e exp_of_ident state_ident] [%e eint idx]]
        in
        let expr_new =
          [%expr
            lazy
              (Model.get
                 (Lazy.force [%e evar new_state_ident.Ident.id_str])
                 [%e eint idx])]
        in
        ( value_binding ~pat:pat_old ~expr:expr_old
          :: value_binding ~pat:pat_new ~expr:expr_new
          :: acc_vbs,
          (sut_var, Ident.create ~loc:Location.none var_old) :: acc_old,
          (sut_var, Ident.create ~loc:Location.none var_new) :: acc_new )
      in
      List.fold_left aux ([], [], []) value.sut_vars
    in
    let wrap e = (if vbs <> [] then pexp_let Nonrecursive vbs e else e) |> ok in
    subst_term state ~gos_t:value.sut_vars ~old_lz:false ~old_t:sut_map_old
      ~new_lz:true ~new_t:sut_map_new t.term
    >>= ocaml_of_term config
    >>= wrap
  and translate_invariants idx sut id t =
    (* translates the type invariants for the given model. As multiple suts may
       be used during a call, idx gives the offset on the stack of suts, sut is
       the identifier for the sut stack, id the name of the model in the
       invariant, and t a term that needs to be checked *)
    let vbs, sut_map =
      let tmp = gen_symbol ~prefix:sut.Ident.id_str () in
      let pat = pvar tmp in
      let expr =
        [%expr
          lazy
            (Model.get
               (Lazy.force [%e evar new_state_ident.Ident.id_str])
               [%e eint idx])]
      in
      (value_binding ~pat ~expr, (id, Ident.create ~loc:Location.none tmp))
    in
    let wrap = pexp_let Nonrecursive [ vbs ] in
    let* subst_term =
      subst_term state ~gos_t:[ id ] ~old_t:[] ~new_t:[ sut_map ] ~new_lz:true
        t.term
    in
    let* ocaml_term = ocaml_of_term config subst_term in
    ocaml_term |> wrap |> ok
  and dummy =
    let ty_show = qualify [ "Ortac_runtime" ] "dummy" and value = eunit in
    let args = pexp_tuple_opt [ ty_show; value ] in
    pexp_construct res args
  in
  let* ret_val =
    (* simply warn the user if we can't compute the expected returned value,
       don't skip the function *)
    match expected_returned_value translate_postcond value with
    | None ->
        (* If the returned value is a SUT, we don't need to ever show it *)
        if Cfg.does_return_sut config value.ty then ok dummy
        else
          let* () =
            warn
              ( Incomplete_ret_val_computation (Fmt.str "%a" Ident.pp value.id),
                value.id.id_loc )
          in
          ok dummy
    | Some e -> ok e
  in
  let wrap_check ?(exn = None) t e =
    let term = estring t.text
    and cmd = Fmt.str "%a" Ident.pp value.id |> estring
    and l = t.Ir.term.Gospel.Tterm.t_loc |> elocation
    and ret_val =
      match exn with
      | Some e -> eexception @@ estring e
      | None ->
          if may_raise_exception value then eprotected ret_val
          else evalue ret_val
    in
    pexp_ifthenelse e enone
      (Some
         (esome
         @@ pexp_apply
              (qualify [ "Ortac_runtime" ] "report")
              [
                ( Nolabel,
                  estring @@ Ortac_core.Context.module_name config.context );
                (Nolabel, estring config.init_sut_txt);
                (Nolabel, ret_val);
                (Nolabel, cmd);
                (Nolabel, elist [ pexp_tuple [ term; l ] ]);
              ]))
  in
  let idx = List.sort Int.compare idx in
  let lhs0 = mk_cmd_pattern value in
  let* lhs1 =
    let ret_ty = Ir.get_return_type value in
    let* ret_ty =
      let open Ppxlib in
      match ret_ty.ptyp_desc with
      | Ptyp_var _ | Ptyp_constr _ | Ptyp_tuple _ -> ok ret_ty
      | _ ->
          error
            ( Type_not_supported (Fmt.str "%a" Pprintast.core_type ret_ty),
              ret_ty.ptyp_loc )
    in
    let* pat_ty =
      if Cfg.is_sut config ret_ty then pvar "SUT" |> ok
      else pat_of_core_type value.inst ret_ty
    in
    let pat_ty =
      if may_raise_exception value then
        ppat_construct (lident "Result")
          (Some (ppat_tuple [ pat_ty; ppat_construct (lident "Exn") None ]))
      else pat_ty
    in
    let pat_ret =
      match value.ret with
      | [] ->
          if may_raise_exception value then pvar (str_of_ident res_default)
          else ppat_any
      | [ id ] -> pvar (str_of_ident id)
      | xs -> ppat_tuple (List.map (fun x -> pvar @@ str_of_ident x) xs)
    in
    ok
      (ppat_construct (lident "Res")
         (Some (ppat_tuple [ ppat_tuple [ pat_ty; ppat_any ]; pat_ret ])))
  in
  let lhs = ppat_tuple [ lhs0; lhs1 ] in
  let* rhs =
    let normal =
      let rec aux idx postcond =
        match (idx, postcond) with
        | [], ps -> List.map snd ps
        | i :: idx, (j, _) :: ps when i = j -> aux idx ps
        | i :: _, (j, p) :: ps ->
            assert (j < i);
            p :: aux idx ps
        | _, _ -> assert false
      in
      aux idx value.postcond.normal
    in
    (* [postcond] and [invariants] are specification of normal behaviour *)
    let* postcond =
      promote_map (fun t -> wrap_check t <$> translate_postcond t) normal
      (* only functions that do not return a sut can have postconditions
         referring to the returned value, therefore no shifting is needed *)
    and* invariants =
      match invariants with
      | None -> ok []
      | Some (id, xs) ->
          let suts =
            if Cfg.does_return_sut config value.ty then
              (* We only check the invariants when the function returned
                 without raising an exception, so in that case we can just
                 prepend the returned sut *)
              let ret_id = List.hd value.ret in
              ret_id :: value.sut_vars
            else value.sut_vars
          in
          promote_mapi
            (fun idx sut ->
              promote_map
                (fun t -> wrap_check t <$> translate_invariants idx sut id t)
                xs)
            suts
    in
    list_append (postcond @ List.concat invariants) |> ok
  in
  let res, pat_ret =
    match value.ret with
    | [] -> (evar (str_of_ident res_default), ppat_any)
    | [ id ] ->
        let id = str_of_ident id in
        (evar id, pvar id)
    | xs ->
        let evars = List.map (fun x -> evar @@ str_of_ident x) xs in
        let pvars = List.map (fun x -> pvar @@ str_of_ident x) xs in
        (pexp_tuple evars, ppat_tuple pvars)
  in
  let* rhs =
    if may_raise_exception value then
      let case_ok =
        case ~lhs:(ppat_construct (lident "Ok") (Some pat_ret)) ~guard:None ~rhs
      in
      let* cases_error =
        Fun.flip ( @ ) [ case ~lhs:ppat_any ~guard:None ~rhs:enone ]
        <$> promote_map
              (fun (x, p, t) ->
                let xstr = Fmt.str "%a" Ident.pp x.Gospel.Ttypes.xs_ident in
                let lhs =
                  ppat_construct (lident xstr)
                    (Option.map Ortac_core.Ocaml_of_gospel.pattern p)
                in
                let lhs = ppat_construct (lident "Error") (Some lhs) in
                let* rhs =
                  wrap_check ~exn:(Some xstr) t <$> translate_postcond t
                in
                case ~lhs ~guard:None ~rhs |> ok)
              value.postcond.exceptional
      in
      pexp_match res (case_ok :: cases_error) |> ok
    else ok rhs
  in
  let* rhs =
    let translate_checks =
      let vbs, sut_map =
        let aux idx sut =
          let tmp_var = gen_symbol ~prefix:"tmp" () in
          let tmp_id = Ident.create ~loc:Location.none tmp_var in
          let tmp_pat = pvar tmp_var in
          let tmp_expr =
            eapply
              (qualify [ "Model" ] "get")
              [
                evar (str_of_ident state_ident);
                pexp_constant (Pconst_integer (string_of_int idx, None));
              ]
          in
          (value_binding ~pat:tmp_pat ~expr:tmp_expr, (sut, tmp_id))
        in
        List.mapi aux value.sut_vars |> List.split
      in
      let wrap e = if vbs <> [] then pexp_let Nonrecursive vbs e else e in
      fun t -> wrap <$> translate_checks config state value sut_map t
    in
    let* checks =
      promote_map
        (fun t ->
          wrap_check ~exn:(Some "Invalid_argument") t <$> translate_checks t)
        value.postcond.checks
    in
    match checks with
    | [] -> ok rhs
    | _ ->
        let inv_arg =
          ppat_construct (lident "Invalid_argument") (Some ppat_any)
        in
        let validate_inv_arg =
          pexp_match res
            [
              case
                ~lhs:(ppat_construct (lident "Error") (Some inv_arg))
                ~guard:None ~rhs:enone;
              case ~lhs:ppat_any ~guard:None ~rhs:(list_append checks);
            ]
        in
        pexp_match (list_append checks)
          [
            case ~lhs:(ppat_construct (lident "None") None) ~guard:None ~rhs;
            case ~lhs:ppat_any ~guard:None ~rhs:validate_inv_arg;
          ]
        |> ok
  in
  ok (case ~lhs ~guard:None ~rhs)

let postcond config idx ir =
  let cmd_name = gen_symbol ~prefix:"cmd" () in
  let state_name = gen_symbol ~prefix:"state" () in
  let res_name = gen_symbol ~prefix:"res" () in
  let new_state_name = gen_symbol ~prefix:"new_state" () in
  let new_state_let =
    pexp_let Nonrecursive
      [
        value_binding ~pat:(pvar new_state_name)
          ~expr:
            (pexp_lazy
               (pexp_apply
                  (pexp_ident (lident "next_state"))
                  [
                    (Nolabel, pexp_ident (lident cmd_name));
                    (Nolabel, pexp_ident (lident state_name));
                  ]));
      ]
  in
  let state_ident = Ident.create ~loc:Location.none state_name in
  let new_state_ident = Ident.create ~loc:Location.none new_state_name in
  let open Reserr in
  let* cases =
    (Fun.flip ( @ )) [ case ~lhs:ppat_any ~guard:None ~rhs:enone ]
    <$> promote_map
          (fun v ->
            postcond_case config ir.state ir.invariants (List.assoc v.id idx)
              state_ident new_state_ident v)
          ir.values
  in
  let body =
    pexp_open
      Ast_helper.(Opn.mk (Mod.ident (lident "Spec")))
      (pexp_open
         Ast_helper.(Opn.mk (Mod.ident (lident "STM")))
         (pexp_match (pexp_tuple [ evar cmd_name; evar res_name ]) cases
         |> new_state_let))
  in
  let pat = pvar "ortac_postcond" in
  let expr =
    efun
      [
        (Nolabel, pvar cmd_name);
        (Nolabel, pvar state_name);
        (Nolabel, pvar res_name);
      ]
      body
  in
  pstr_value Nonrecursive [ value_binding ~pat ~expr ] |> ok

let dummy_postcond =
  let expr =
    efun
      [ (Nolabel, ppat_any); (Nolabel, ppat_any); (Nolabel, ppat_any) ]
      (ebool true)
  and pat = pvar "postcond" in
  pstr_value Nonrecursive [ value_binding ~pat ~expr ]

let cmd_constructor value =
  let name = String.capitalize_ascii value.id.Ident.id_str |> noloc in
  let args =
    List.map (fun (ty, _) -> subst_core_type value.inst ty) value.args
  in
  constructor_declaration ~name ~args:(Pcstr_tuple args) ~res:None

let cmd_type ir =
  let constructors = List.map cmd_constructor ir.values in
  let td =
    type_declaration ~name:(noloc "cmd") ~params:[] ~cstrs:[]
      ~kind:(Ptype_variant constructors) ~private_:Public ~manifest:None
  in
  pstr_type Recursive [ td ]

let pp_cmd_case config value =
  let lhs = mk_cmd_pattern value in
  let qualify_pp = qualify [ "Util"; "Pp" ] in
  let get_name =
    Option.fold ~none:eunit ~some:(fun id -> str_of_ident id |> evar)
  in
  let open Reserr in
  let rec pp_of_ty ty : expression reserr =
    match ty.ptyp_desc with
    | Ptyp_tuple xs ->
        let* pps = promote_map pp_of_ty xs in
        let func = qualify_pp ("pp_tuple" ^ string_of_int (List.length xs)) in
        ok (pexp_apply func (List.map (fun e -> (Nolabel, e)) pps))
    | Ptyp_constr (lid, xs) ->
        let* xs = promote_map pp_of_ty xs
        and* s = munge_longident false ty lid in
        let pp = qualify_pp ("pp_" ^ s) in
        ok
          (match xs with
          | [] -> pp
          | _ -> pexp_apply pp (List.map (fun x -> (Nolabel, x)) xs))
    | _ ->
        error
          (Type_not_supported (Fmt.str "%a" Pprintast.core_type ty), ty.ptyp_loc)
  in
  let* rhs =
    let name = str_of_ident value.id in
    let rec aux ty args =
      match (ty.ptyp_desc, args) with
      | Ptyp_arrow (_, l, r), xs when Cfg.is_sut config l ->
          let* fmt, pps = aux r xs in
          ok ("<sut>" :: fmt, pps)
      | Ptyp_arrow (_, _, r), (ty, id) :: xs ->
          let ty = subst_core_type value.inst ty in
          let* pp = pp_of_ty ty and* fmt, pps = aux r xs in
          ok
            ( "%a" :: fmt,
              pexp_apply pp [ (Nolabel, ebool true) ] :: get_name id :: pps )
      | _, [] -> ok ([], [])
      | _, _ ->
          failwith
            "shouldn't happen (list of arguments should be consistent with \
             type)"
    in
    let* fmt, pp_args = aux value.ty value.args in
    let fmt =
      let call = String.concat " " ("%s" :: fmt) in
      if may_raise_exception value then "protect (fun () -> " ^ call ^ ")"
      else call
    in
    let args =
      List.map (fun x -> (Nolabel, x)) (estring fmt :: estring name :: pp_args)
    in
    pexp_apply (qualify [ "Format" ] "asprintf") args |> ok
  in
  case ~lhs ~guard:None ~rhs |> ok

let cmd_show config ir =
  let cmd_name = gen_symbol ~prefix:"cmd" () in
  let open Reserr in
  let* cases = promote_map (pp_cmd_case config) ir.values in
  let body = pexp_match (evar cmd_name) cases in
  let pat = pvar "show_cmd" in
  let expr = efun [ (Nolabel, pvar cmd_name) ] body in
  pstr_value Nonrecursive [ value_binding ~pat ~expr ] |> ok

let get_max_suts ir =
  List.fold_left
    (fun curr value ->
      let len = List.length value.sut_vars in
      if len > curr then len else curr)
    0 ir.values

let sut_module cfg =
  let init_sut = [%stri let init () = [%e cfg.Cfg.init_sut]] in
  let td =
    type_declaration ~name:(noloc "sut") ~params:[] ~cstrs:[]
      ~kind:Ptype_abstract ~private_:Public
      ~manifest:(Some cfg.Cfg.sut_core_type)
  in
  let sut_ty = pstr_type Recursive [ td ] in
  [%str
    module SUT = Ortac_runtime.SUT.Make (struct
      [%%i sut_ty]
      [%%i init_sut]
    end)]

let sut_defs ir =
  let max_suts = get_max_suts ir in
  [
    [%stri type sut = SUT.t];
    [%stri let init_sut = SUT.create [%e eint max_suts]];
  ]

(* This function generates an expression of the form
   ```
   let a = expr0 in
   let b = expr1 in
   { field0 = (translation of a gospel term using a and/or b; ... }
   ```
*)
let init_state config ir =
  let pat_of_lb_arg = function
    (* here we don't need the labels as we'll use them in the body of the function *)
    | Gospel.Tast.Lunit -> punit
    | Gospel.Tast.Lnone vs
    | Gospel.Tast.Loptional vs
    | Gospel.Tast.Lnamed vs
    | Gospel.Tast.Lghost vs ->
        pvar (Fmt.str "%a" Ident.pp vs.vs_name)
  in
  let bindings =
    pexp_let Nonrecursive
      (List.map
         (fun (lb_arg, expr) -> value_binding ~pat:(pat_of_lb_arg lb_arg) ~expr)
         ir.Ir.init_state.arguments)
  in
  let open Reserr in
  let translate_field_desc Ir.{ model; description } =
    let* desc =
      subst_term ir.state
        ~gos_t:[ ir.init_state.returned_sut ]
        ~old_t:[] ~new_t:[] description
      >>= ocaml_of_term config
    in
    ok (model, desc)
  in
  let* fields =
    promote_map translate_field_desc ir.Ir.init_state.descriptions
  in
  let* fields =
    promote_map
      (fun (id, _) ->
        (fun d -> (longident_loc_of_ident id, d))
        <$> (List.assoc_opt id fields
            |> of_option
                 ~default:
                   ( Impossible_init_state_generation
                       (No_translatable_specification id.Ident.id_str),
                     Ppxlib.Location.none )))
      ir.state
  in
  let expr = pexp_record fields None |> bindings in
  [%stri let init = [%e expr]] |> ok

let model_module config ir =
  let lds =
    List.map
      (fun (id, ty) ->
        label_declaration
          ~name:(Fmt.str "%a" Ident.pp id |> noloc)
          ~mutable_:Immutable ~type_:ty)
      ir.state
  in
  let kind = Ptype_record lds in
  let td =
    type_declaration ~name:(noloc "elt") ~params:[] ~cstrs:[] ~kind
      ~private_:Public ~manifest:None
  in
  let state_ty = pstr_type Nonrecursive [ td ] in
  let open Reserr in
  let* init_state = init_state config ir in
  [
    [%stri
      module ModelElt = struct
        [%%i state_ty]
        [%%i init_state]
      end];
    [%stri module Model = Ortac_runtime.Model.Make (ModelElt)];
  ]
  |> ok

let state_defs ir =
  let max_suts = get_max_suts ir in
  [
    [%stri type state = Model.t];
    [%stri let init_state = Model.create [%e eint max_suts] ()];
  ]

let check_init_state config ir =
  let init_state = [%expr Model.get Spec.init_state 0] in
  let open Reserr in
  let state_name = gen_symbol ~prefix:"__state" () in
  let state_pat = pvar state_name
  and state_id = Ident.create ~loc:Location.none state_name in
  let translate_invariants id t =
    enot
    <$> (subst_term ir.state ~gos_t:[ id ] ~old_t:[]
           ~new_t:[ (id, state_id) ]
           t.term
        >>= ocaml_of_term config)
  and msg =
    let f = qualify [ "QCheck"; "Test" ] "fail_report"
    and s = estring "INIT_SUT violates type invariants for SUT" in
    eapply f [ s ]
  in
  let* expr =
    (function
      | [] -> eunit
      | xs ->
          pexp_let Nonrecursive
            [ value_binding ~pat:state_pat ~expr:init_state ]
            (pexp_ifthenelse (list_or xs) msg None))
    <$> Option.fold ~none:(ok [])
          ~some:(fun (id, xs) -> promote_map (translate_invariants id) xs)
          ir.invariants
  in
  let pat = pvar "check_init_state" and expr = efun [ (Nolabel, punit) ] expr in
  pstr_value Nonrecursive [ value_binding ~pat ~expr ] |> ok

let ghost_function config fct =
  let open Gospel in
  let open Tast in
  let open Reserr in
  match fct.fun_def with
  | None -> failwith "impossible"
  | Some t ->
      let name = str_of_ident fct.fun_ls.ls_name in
      let config' =
        Cfg.
          {
            config with
            context =
              Ortac_core.Context.add_function fct.fun_ls name config.context;
          }
      in
      let* body = ocaml_of_term (if fct.fun_rec then config' else config) t in
      let body =
        efun
          (List.map
             (fun vs -> (Nolabel, pvar (str_of_ident vs.Symbols.vs_name)))
             fct.fun_params)
          body
      in
      let bindings = [ value_binding ~pat:(pvar name) ~expr:body ] in
      ( config',
        pstr_value (if fct.fun_rec then Recursive else Nonrecursive) bindings )
      |> ok

let ghost_functions config =
  let open Reserr in
  let rec aux config (acc : structure) = function
    | [] -> ok (config, List.rev acc)
    | fct :: xs -> (
        let* f = promote_opt (ghost_function config fct) in
        match f with
        | None -> aux config acc xs
        | Some (config, f) -> aux config (f :: acc) xs)
  in
  aux config []

let ghost_types config =
  let open Reserr in
  let aux (rec_flag, type_decls) =
    let rec_flag =
      match rec_flag with
      | Gospel.Tast.Nonrecursive -> Nonrecursive
      | Gospel.Tast.Recursive -> Recursive
    in
    let* tds =
      promote_map
        (fun td ->
          try
            ok
              (Ortac_core.Ocaml_of_gospel.ocaml_type_decl_of_gospel_type_decl
                 ~context:config.Cfg.context td)
          with W.Error e -> error e)
        type_decls
    in
    ok (pstr_type rec_flag tds)
  in
  promote_map aux

let agree_prop =
  [%stri
    let agree_prop cs =
      check_init_state ();
      STMTests.agree_prop cs]

let prepend_include_in_module name lident structure =
  let open Ast_helper in
  let name = noloc (Some name)
  and expr =
    pmod_structure
    @@ ((Mod.ident lident |> Incl.mk |> pstr_include) :: structure)
  in
  [ pstr_module @@ module_binding ~name ~expr ]

let qcheck config =
  match config.Cfg.gen_mod with
  | None -> []
  | Some structure ->
      let structure =
        prepend_include_in_module "Gen" (lident "Gen") structure
      in
      prepend_include_in_module "QCheck" (lident "QCheck") structure

let util config =
  match config.Cfg.pp_mod with
  | None -> []
  | Some structure ->
      let structure =
        prepend_include_in_module "Pp"
          (noloc (Ldot (Lident "Util", "Pp")))
          structure
      in
      (* We don't need the whole `Util` module here *)
      let name = noloc (Some "Util") and expr = pmod_structure structure in
      [ pstr_module (module_binding ~name ~expr) ]

let gen_tuple_ty arities =
  let constructors =
    List.map
      (fun ar ->
        let name = Located.mk ("Tup" ^ string_of_int ar) in
        let idxs =
          List.init ar (fun x -> "a" ^ string_of_int (x + 1) |> ptyp_var)
        in
        let args = List.map (fun c -> ptyp_constr (lident "ty") [ c ]) idxs in
        let ret = ptyp_constr (lident "ty") [ ptyp_tuple idxs ] in
        let kind = Pext_decl ([], Pcstr_tuple args, Some ret) in
        extension_constructor ~name ~kind)
      arities
  in
  let path = lident "ty" in
  let params = [ (ptyp_any, (NoVariance, NoInjectivity)) ] in
  let private_ = Public in
  pstr_typext (type_extension ~path ~params ~constructors ~private_)

let gen_tuple_constr arities =
  let range idx = List.init idx (fun x -> x + 1) in
  let gen_vb arity =
    let arity_str = string_of_int arity in
    let idxs = range arity in
    let pat = pvar ("tup" ^ arity_str) in
    let ty_show =
      pexp_tuple
        [
          pexp_construct
            (lident ("Tup" ^ arity_str))
            (Some
               (pexp_tuple
                  (List.map (fun i -> evar ("ty" ^ string_of_int i)) idxs)));
          pexp_apply
            (qualify [ "Util"; "Pp" ] "to_show")
            [
              ( Nolabel,
                pexp_apply
                  (qualify [ "Util"; "Pp" ] ("pp_tuple" ^ string_of_int arity))
                  (List.map
                     (fun i ->
                       ( Nolabel,
                         pexp_apply
                           (qualify [ "Util"; "Pp" ] "of_show")
                           [ (Nolabel, evar ("show" ^ string_of_int i)) ] ))
                     idxs) );
            ];
        ]
    in
    let body =
      pexp_let Nonrecursive
        (List.map
           (fun i ->
             let pat =
               ppat_tuple
                 [
                   pvar ("ty" ^ string_of_int i); pvar ("show" ^ string_of_int i);
                 ]
             in
             let expr = evar ("spec" ^ string_of_int i) in
             value_binding ~pat ~expr)
           idxs)
        ty_show
    in
    let expr =
      List.fold_left
        (fun acc i ->
          pexp_fun Nolabel None (pvar ("spec" ^ string_of_int i)) acc)
        body (List.rev idxs)
    in
    value_binding ~pat ~expr
  in
  let vbs = List.map gen_vb arities in
  pstr_value Nonrecursive vbs

(* This function creates type extensions for STM.ty and smart constructors for
   them. It looks up all required tuple arities from the return types of
   IR values
*)
let tuple_types ir =
  let ret_tys =
    List.map (fun v -> (Ir.get_return_type v).ptyp_desc) ir.values
  in
  (* We use a set as we only need each tuple arity once *)
  let module IntS = Set.Make (Int) in
  let rec aux acc = function
    | Ptyp_tuple xs ->
        let acc' =
          (* Tuples might be nested *)
          List.fold_left aux acc (List.map (fun x -> x.ptyp_desc) xs)
        in
        IntS.union (IntS.singleton (List.length xs)) acc'
    | _ -> acc
  in
  let arities = List.fold_left aux IntS.empty ret_tys |> IntS.elements in
  if List.length arities = 0 then []
  else [ gen_tuple_ty arities; gen_tuple_constr arities ]

let integer_ty_ext =
  [
    [%stri type _ ty += Integer : Ortac_runtime.integer ty];
    [%stri let integer = (Integer, Ortac_runtime.string_of_integer)];
  ]

let pp_ortac_cmd_case config suts last value =
  let open Reserr in
  let lhs0 = mk_cmd_pattern value in
  let* lhs1 =
    let ret_ty = Ir.get_return_type value in
    let* ret_ty =
      let open Ppxlib in
      match ret_ty.ptyp_desc with
      | Ptyp_var _ | Ptyp_constr _ | Ptyp_tuple _ -> ok ret_ty
      (* Unsupported return types are already filtered out *)
      | _ -> assert false
    in
    let* pat_ty =
      if Cfg.is_sut config ret_ty then pvar "SUT" |> ok
      else pat_of_core_type value.inst ret_ty
    in
    let pat_ty =
      if may_raise_exception value then
        ppat_construct (lident "Result")
          (Some (ppat_tuple [ pat_ty; ppat_construct (lident "Exn") None ]))
      else pat_ty
    in
    let pat_ret =
      if Cfg.does_return_sut config value.ty then
        List.hd value.ret |> str_of_ident |> pvar
      else ppat_any
    in
    ok
      (ppat_construct (lident "Res")
         (Some (ppat_tuple [ ppat_tuple [ pat_ty; ppat_any ]; pat_ret ])))
  in
  let lhs = ppat_tuple [ lhs0; lhs1 ] in
  let qualify_pp = qualify [ "Util"; "Pp" ] in
  let get_name =
    Option.fold ~none:eunit ~some:(fun id -> str_of_ident id |> evar)
  in
  let open Reserr in
  let rec pp_of_ty ty : expression reserr =
    match ty.ptyp_desc with
    | Ptyp_tuple xs ->
        let* pps = promote_map pp_of_ty xs in
        let func = qualify_pp ("pp_tuple" ^ string_of_int (List.length xs)) in
        ok (pexp_apply func (List.map (fun e -> (Nolabel, e)) pps))
    | Ptyp_constr (lid, xs) ->
        let* xs = promote_map pp_of_ty xs
        and* s = munge_longident false ty lid in
        let pp = qualify_pp ("pp_" ^ s) in
        ok
          (match xs with
          | [] -> pp
          | _ -> pexp_apply pp (List.map (fun x -> (Nolabel, x)) xs))
    | _ ->
        error
          (Type_not_supported (Fmt.str "%a" Pprintast.core_type ty), ty.ptyp_loc)
  in
  let* rhs =
    let name = str_of_ident value.id in
    let rec aux ty n args =
      match (ty.ptyp_desc, args) with
      | Ptyp_arrow (_, l, r), xs when Cfg.is_sut config l ->
          let* fmt, pps = aux r (n + 1) xs in
          let get_sut =
            eapply
              (qualify [ "SUT" ] "get_name")
              [ evar suts; eapply (evar "+") [ eint n; evar "shift" ] ]
          in
          ok ("%s" :: fmt, get_sut :: pps)
      | Ptyp_arrow (_, _, r), (ty, id) :: xs ->
          let ty = subst_core_type value.inst ty in
          let* pp = pp_of_ty ty and* fmt, pps = aux r n xs in
          ok
            ( "%a" :: fmt,
              pexp_apply pp [ (Nolabel, ebool true) ] :: get_name id :: pps )
      | _, [] -> ok ([], [])
      | _, _ ->
          failwith
            "shouldn't happen (list of arguments should be consistent with \
             type)"
    in
    let* fmt, pp_args = aux value.ty 0 value.args in
    let fmt =
      let call = String.concat " " ("%s" :: fmt) in
      let protected_call =
        if may_raise_exception value then "protect (fun () -> " ^ call ^ ")"
        else call
      in
      "let %s = " ^ protected_call
    in
    let args =
      List.map
        (fun x -> (Nolabel, x))
        (estring fmt :: evar "lhs" :: estring name :: pp_args)
    in
    let res_match ok_expr error_expr =
      let res =
        match value.ret with
        | [ id ] -> evar (str_of_ident id)
        | _ -> failwith "should not happen, can only return exactly one sut"
      in
      pexp_match res
        [
          (case ~lhs:(ppat_construct (lident "Ok") (Some ppat_any)))
            ~guard:None ~rhs:ok_expr;
          (case ~lhs:(ppat_construct (lident "Error") (Some ppat_any)))
            ~guard:None ~rhs:error_expr;
        ]
    in
    let lhs =
      let pat = pvar "lhs" in
      let expr =
        pexp_ifthenelse (evar last) (estring "r")
          (Some
             (if
                Cfg.does_return_sut config value.ty && may_raise_exception value
              then
                res_match
                  [%expr "Ok " ^ SUT.get_name [%e evar suts] 0]
                  (estring "_")
              else if Cfg.does_return_sut config value.ty then
                eapply (qualify [ "SUT" ] "get_name") [ evar suts; eint 0 ]
              else estring "_"))
      in
      value_binding ~pat ~expr
    in
    let shift =
      let pat = pvar "shift" in
      let expr =
        if Cfg.does_return_sut config value.ty && may_raise_exception value then
          res_match (eint 1) (eint 0)
        else if Cfg.does_return_sut config value.ty then eint 1
        else eint 0
      in
      value_binding ~pat ~expr
    in
    pexp_let Nonrecursive [ lhs; shift ]
      (pexp_apply (qualify [ "Format" ] "asprintf") args)
    |> ok
  in
  case ~lhs ~guard:None ~rhs |> ok

let ortac_cmd_show config ir =
  let cmd_name = gen_symbol ~prefix:"cmd" () in
  let suts_name = gen_symbol ~prefix:"state" () in
  let res_name = gen_symbol ~prefix:"res" () in
  let last_name = gen_symbol ~prefix:"last" () in
  let open Reserr in
  let* cases =
    promote_map (pp_ortac_cmd_case config suts_name last_name) ir.values
  in
  let default_case =
    case ~lhs:ppat_any ~guard:None ~rhs:(eapply (evar "assert") [ ebool false ])
  in
  let cases = cases @ [ default_case ] in
  let match_expr =
    pexp_match (pexp_tuple [ evar cmd_name; evar res_name ]) cases
  in
  let body =
    pexp_open
      Ast_helper.(Opn.mk (Mod.ident (lident "Spec")))
      (pexp_open Ast_helper.(Opn.mk (Mod.ident (lident "STM"))) match_expr)
  in
  let pat = pvar "ortac_show_cmd" in
  let expr =
    efun
      [
        (Nolabel, pvar cmd_name);
        (Nolabel, pvar suts_name);
        (Nolabel, pvar last_name);
        (Nolabel, pvar res_name);
      ]
      body
  in
  pstr_value Nonrecursive [ value_binding ~pat ~expr ] |> ok

let sut_stm_ty_defs config ir =
  let ret_vals = List.map Ir.get_return_type ir.values in
  let has_sut_ret = List.exists (Cfg.is_sut config) ret_vals in
  if has_sut_ret then
    [%str
      type _ ty += SUT : SUT.elt ty

      let sut = (SUT, fun _ -> "<sut>")]
  else []

let stm config ir =
  let open Reserr in
  let* ghost_types = ghost_types config ir.ghost_types in
  let* config, ghost_functions = ghost_functions config ir.ghost_functions in
  let warn = [%stri [@@@ocaml.warning "-26-27-69-32-38"]] in
  let cmd = cmd_type ir in
  let* cmd_show = cmd_show config ir in
  let* idx, next_state = next_state config ir in
  let* postcond = postcond config idx ir in
  let* precond = precond config ir in
  let* run = run config ir in
  let* arb_cmd = arb_cmd config ir in
  let* check_init_state = check_init_state config ir in
  let* ortac_show = ortac_cmd_show config ir in
  let cleanup =
    let default =
      let pat = pvar "cleanup" in
      let expr = efun [ (Nolabel, ppat_any) ] eunit in
      pstr_value Nonrecursive [ value_binding ~pat ~expr ]
    in
    Option.value config.cleanup ~default
  in
  let open_mod m = pstr_open Ast_helper.(Opn.mk (Mod.ident (lident m))) in
  let sut_defs = sut_defs ir in
  let state_defs = state_defs ir in
  let spec_expr =
    pmod_structure
      ((open_mod "STM" :: qcheck config)
      @ util config
      @ Option.value config.ty_mod ~default:[]
      @ integer_ty_ext
      @ sut_stm_ty_defs config ir
      @ tuple_types ir
      @ sut_defs
      @ state_defs
      @ [
          cmd;
          cmd_show;
          cleanup;
          arb_cmd;
          next_state;
          precond;
          dummy_postcond;
          run;
        ])
  in
  let stm_spec =
    pstr_module (module_binding ~name:(noloc (Some "Spec")) ~expr:spec_expr)
  in
  let tests =
    pstr_module
      (module_binding ~name:(noloc (Some "STMTests"))
         ~expr:
           (pmod_apply
              (pmod_ident (Ldot (Lident "Ortac_runtime", "Make") |> noloc))
              (pmod_ident (lident "Spec"))))
  in
  let module_name = Ortac_core.Context.module_name config.context in
  let call_tests =
    let loc = Location.none in
    let descr = estring (module_name ^ " STM tests") in
    let max_suts = get_max_suts ir in
    [%stri
      let _ =
        QCheck_base_runner.run_tests_main
          (let count = 1000 in
           [
             STMTests.agree_test ~count ~name:[%e descr] [%e eint max_suts]
               check_init_state ortac_show_cmd ortac_postcond;
           ])]
  in
  let sut_mod = sut_module config in
  let* model_mod = model_module config ir in
  ok
    (warn
     :: open_mod module_name
     :: [%stri module Ortac_runtime = Ortac_runtime_qcheck_stm]
     :: ghost_types
    @ ghost_functions
    @ sut_mod
    @ model_mod
    @ [ stm_spec; tests; check_init_state; ortac_show; postcond; call_tests ])

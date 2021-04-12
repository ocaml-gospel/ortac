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

module Make_Builder (F : G) = struct
  include Ast_builder.Make (struct
    let loc = Location.none
  end)

  let noloc txt = { txt; loc = Location.none }

  let epred e = eapply (evar "Z.pred") [ e ]

  let esucc e = eapply (evar "Z.succ") [ e ]

  let econst = function
    | Pconst_integer (c, o) ->
        Pconst_integer (c, o) |> pexp_constant |> fun e ->
        eapply (evar "Z.of_int") [ e ]
    | _ as e -> pexp_constant e

  let eposition pos =
    [%expr
      {
        pos_fname = [%e estring pos.pos_fname];
        pos_lnum = [%e eint pos.pos_lnum];
        pos_bol = [%e eint pos.pos_bol];
        pos_cnum = [%e eint pos.pos_cnum];
      }]

  let elocation loc =
    [%expr
      let open Ppxlib.Location in
      {
        loc_start = [%e eposition loc.loc_start];
        loc_end = [%e eposition loc.loc_end];
        loc_ghost = [%e ebool loc.loc_ghost];
      }]

  let failed error_kind term_kind acc_name fun_name (eloc : expression) term :
      expression =
    let term =
      pexp_construct
        (noloc
           (lident
              (match term_kind with
              | `Pre -> "Pre"
              | `Post -> "Post"
              | `XPost -> "XPost")))
        (Some (estring (Fmt.str "%a" Gospel.Tterm.print_term term)))
    in
    match error_kind with
    | `Violated ->
        [%expr
          let err =
            mk_condition [%e eloc] [%e estring fun_name] [%e term] Violated
          in
          Errors.register err [%e evar acc_name]]
    | `RuntimeExn e ->
        [%expr
          let err =
            mk_condition [%e eloc] [%e estring fun_name] [%e term]
              (RuntimeExn [%e e])
          in
          Errors.register err [%e evar acc_name];
          true]

  let failed_pre = failed `Violated `Pre

  let failed_post = failed `Violated `Post

  let failed_pre_nonexec acc_name exn = failed (`RuntimeExn exn) `Pre acc_name

  let failed_post_nonexec acc_name exn = failed (`RuntimeExn exn) `Post acc_name

  let failed_xpost_nonexec acc_name exn =
    failed (`RuntimeExn exn) `XPost acc_name

  let efun args expr =
    List.fold_right
      (fun a ->
        let label, p = a in
        pexp_fun label None p)
      args expr

  exception Unsupported of Location.t option * string

  let lident s = noloc (lident s)

  let rec pattern p =
    match p.Tterm.p_node with
    | Tterm.Pwild -> ppat_any
    | Tterm.Pvar v -> pvar (str "%a" Identifier.Ident.pp v.vs_name)
    | Tterm.Papp (l, pl) when Tterm.is_fs_tuple l ->
        ppat_tuple (List.map pattern pl)
    | Tterm.Papp (l, pl) ->
        let args =
          if pl = [] then None else Some (ppat_tuple (List.map pattern pl))
        in
        ppat_construct (lident l.ls_name.id_str) args
    | Tterm.Por (p1, p2) -> ppat_or (pattern p1) (pattern p2)
    | Tterm.Pas (p, v) ->
        ppat_alias (pattern p) (noloc (str "%a" Identifier.Ident.pp v.vs_name))

  let rec array_no_coercion (ls : Tterm.lsymbol) (tlist : Tterm.term list) =
    (match ls.ls_name.id_str with
    | "mixfix [_]" -> Some "Array.get"
    | "length" -> Some "Array.length"
    | _ -> None)
    |> Option.map (fun f ->
           match (List.hd tlist).t_node with
           | Tapp (elts, [ arr ]) when elts.ls_name.id_str = "elts" ->
               Some
                 (eapply (evar f) (term arr :: List.map term (List.tl tlist)))
           | _ -> None)
    |> Option.join

  and bounds (var : Tterm.vsymbol) (t : Tterm.term) :
      (expression * expression) option =
    (* [comb] extracts a bound from an the operator [f] and expression [e].
       [right] indicates if [e] is on the right side of the operator. *)
    let comb ~right (f : Tterm.lsymbol) e =
      match f.ls_name.id_str with
      | "infix >=" -> if right then (Some e, None) else (None, Some e)
      | "infix <=" -> if right then (None, Some e) else (Some e, None)
      | "infix <" ->
          if right then (None, Some (epred e)) else (Some (esucc e), None)
      | "infix >" ->
          if right then (Some (esucc e), None) else (None, Some (epred e))
      | _ -> (None, None)
    in
    let bound = function
      | Tterm.Tapp (f, [ x1; x2 ]) -> (
          match (x1, x2) with
          | { Tterm.t_node = Tvar { vs_name; _ }; _ }, _
            when vs_name = var.vs_name ->
              let e2 = term x2 in
              comb ~right:true f e2
          | _, { Tterm.t_node = Tvar { vs_name; _ }; _ }
            when vs_name = var.vs_name ->
              let e1 = term x1 in
              comb ~right:false f e1
          | _, _ -> (None, None))
      | _ -> (None, None)
    in
    match t.t_node with
    | Tbinop (Tand, t1, t2) -> (
        match (bound t1.t_node, bound t2.t_node) with
        | (None, Some eupper), (Some elower, None)
        | (Some elower, None), (None, Some eupper) ->
            Some (elower, eupper)
        | _, _ -> None
        | exception Unsupported _ -> None)
    | _ -> None

  and term (t : Tterm.term) : expression =
    let unsupported m = raise (Unsupported (t.t_loc, m)) in
    match t.t_node with
    | Tvar { vs_name; _ } -> evar (str "%a" Identifier.Ident.pp vs_name)
    | Tconst c -> econst c
    | Tapp (fs, []) when Tterm.(ls_equal fs fs_bool_true) -> [%expr true]
    | Tapp (fs, []) when Tterm.(ls_equal fs fs_bool_false) -> [%expr false]
    | Tapp (fs, tlist) when Tterm.is_fs_tuple fs ->
        List.map term tlist |> pexp_tuple
    | Tapp (ls, tlist) -> (
        match array_no_coercion ls tlist with
        | Some e -> e
        | None -> (
            let func = ls.ls_name.id_str in
            Drv.find_opt func |> function
            | Some f -> eapply (evar f) (List.map term tlist)
            | None ->
                if ls.ls_constr then
                  (if tlist = [] then None
                  else Some (List.map term tlist |> pexp_tuple))
                  |> pexp_construct (lident func)
                else kstr unsupported "function application `%s`" func))
    | Tif (i, t, e) -> [%expr if [%e term i] then [%e term t] else [%e term e]]
    | Tlet (x, t1, t2) ->
        let x = str "%a" Identifier.Ident.pp x.vs_name in
        [%expr
          let [%p pvar x] = [%e term t1] in
          [%e term t2]]
    | Tcase (t, ptl) ->
        List.map
          (fun (p, t) -> case ~guard:None ~lhs:(pattern p) ~rhs:(term t))
          ptl
        |> pexp_match (term t)
    | Tquant (quant, [ var ], _, t) -> (
        match quant with
        | Tterm.Tforall | Tterm.Texists -> (
            let z_op = if quant = Tforall then "forall" else "exists" in
            let gospel_op = function
              | Tterm.Timplies -> quant = Tforall
              | Tterm.Tand | Tand_asym -> quant = Texists
              | _ -> false
            in
            match t.t_node with
            | Tbinop (op, t1, t2) when gospel_op op -> (
                bounds var t1 |> function
                | None -> unsupported "forall/exists"
                | Some (start, stop) ->
                    let t2 = term t2 in
                    let x = str "%a" Identifier.Ident.pp var.vs_name in
                    let func = pexp_fun Nolabel None (pvar x) t2 in
                    eapply (evar (str "Z.%s" z_op)) [ start; stop; func ])
            | Tterm.Ttrue -> [%expr true]
            | Tterm.Tfalse -> [%expr false]
            | _ -> unsupported z_op)
        | Tterm.Tlambda -> unsupported "lambda quantification")
    | Tquant (_, _, _, _) -> unsupported "forall/exists with multiple variables"
    | Tbinop (op, t1, t2) -> (
        match op with
        | Tterm.Tand ->
            let vt1 = gen_symbol ~prefix:"__t1" () in
            let vt2 = gen_symbol ~prefix:"__t2" () in
            [%expr
              let [%p pvar vt1] = [%e term t1] in
              let [%p pvar vt2] = [%e term t2] in
              [%e evar vt1] && [%e evar vt2]]
        | Tterm.Tand_asym -> [%expr [%e term t1] && [%e term t2]]
        | Tterm.Tor ->
            let vt1 = gen_symbol ~prefix:"__t1" () in
            let vt2 = gen_symbol ~prefix:"__t2" () in
            [%expr
              let [%p pvar vt1] = [%e term t1] in
              let [%p pvar vt2] = [%e term t2] in
              [%e evar vt1] || [%e evar vt2]]
        | Tterm.Tor_asym -> [%expr [%e term t1] || [%e term t2]]
        | Tterm.Timplies -> [%expr (not [%e term t1]) || [%e term t2]]
        | Tterm.Tiff -> [%expr [%e term t1] = [%e term t2]])
    | Tnot t -> [%expr not [%e term t]]
    | Told _ -> unsupported "old operator"
    | Ttrue -> [%expr true]
    | Tfalse -> [%expr false]

  let term fail t = [%expr try [%e term t] with e -> [%e fail (evar "e")]]

  let conditions fail_violated fail_nonexec terms =
    List.map
      (fun t ->
        [%expr if not [%e term (fail_nonexec t) t] then [%e fail_violated t]])
      terms
    |> esequence

  let post acc_name fun_name eloc =
    let fail_violated t = failed_post acc_name fun_name eloc t in
    let fail_nonexec t e = failed_post_nonexec acc_name e fun_name eloc t in
    conditions fail_violated fail_nonexec

  let pre acc_name fun_name eloc pres =
    let fail_violated t = failed_pre acc_name fun_name eloc t in
    let fail_nonexec t e = failed_pre_nonexec acc_name e fun_name eloc t in
    conditions fail_violated fail_nonexec pres

  let rec xpost_pattern exn = function
    | Tterm.Pwild -> ppat_construct (lident exn) (Some ppat_any)
    | Tterm.Pvar x ->
        ppat_construct (lident exn)
          (Some (ppat_var (noloc (str "%a" Tterm.Ident.pp x.vs_name))))
    | Tterm.Papp (ls, []) when Tterm.(ls_equal ls (fs_tuple 0)) -> pvar exn
    | Tterm.Papp (_ls, _l) -> assert false
    | Tterm.Por (p1, p2) ->
        ppat_or (xpost_pattern exn p1.p_node) (xpost_pattern exn p2.p_node)
    | Tterm.Pas (p, s) ->
        ppat_alias
          (xpost_pattern exn p.p_node)
          (noloc (str "%a" Tterm.Ident.pp s.vs_name))

  let xpost_guard acc_name _loc fun_name eloc xpost call =
    let module M = Map.Make (struct
      type t = Ttypes.xsymbol

      let compare = compare
    end) in
    let default_cases =
      (* undeclared exn *)
      [
        case ~guard:None
          ~lhs:[%pat? e]
          ~rhs:[%expr [%e F.report_undeclared_exn eloc fun_name acc_name]];
      ]
    in
    let assert_false_case =
      case ~guard:None ~lhs:[%pat? _] ~rhs:[%expr assert false]
    in
    List.fold_left
      (fun map (exn, ptlist) ->
        let name = exn.Ttypes.xs_ident.id_str in
        let cases =
          List.rev_map
            (fun (p, t) ->
              let fail_nonexec e =
                failed_xpost_nonexec acc_name e fun_name eloc t
              in
              case ~guard:None
                ~lhs:(xpost_pattern name p.Tterm.p_node)
                ~rhs:
                  [%expr
                    if not [%e term fail_nonexec t] then
                      [%e failed_post acc_name fun_name eloc t]])
            ptlist
          @ [ assert_false_case ]
        in
        M.update exn
          (function None -> Some [ cases ] | Some e -> Some (cases :: e))
          map)
      M.empty xpost
    |> fun cases ->
    M.fold
      (fun exn cases acc ->
        let name = exn.Ttypes.xs_ident.id_str in
        let has_args = exn.Ttypes.xs_type <> Ttypes.Exn_tuple [] in
        let alias = gen_symbol ~prefix:"__e" () in
        let rhs =
          (* after pattern matching on declared exn *)
          [%expr
            [%e List.map (pexp_match (evar alias)) cases |> esequence];
            [%e F.report_declared_exn acc_name alias]]
        in
        let lhs =
          ppat_alias
            (ppat_construct (lident name)
               (if has_args then Some ppat_any else None))
            (noloc alias)
        in
        case ~guard:None ~lhs ~rhs :: acc)
      cases default_cases
    |> pexp_try call

  let returned_pattern rets =
    let to_string x = str "%a" Tast.Ident.pp x.Tterm.vs_name in
    let pvars, evars =
      List.filter_map
        (function
          | Tast.Lunit -> Some (punit, eunit)
          | Tast.Lnone x ->
              let s = to_string x in
              Some (pvar s, evar s)
          | Tast.Lghost _ -> None
          | Tast.Loptional _ | Tast.Lnamed _ -> assert false)
        rets
      |> List.split
    in
    (ppat_tuple pvars, pexp_tuple evars)

  let mk_open = F.open_modules

  let mk_setup loc =
    let loc_name = gen_symbol ~prefix:"__loc" () in
    let let_loc next =
      [%expr
        let [%p pvar loc_name] = [%e elocation loc] in
        [%e next]]
    in
    let acc_name = gen_symbol ~prefix:"__acc" () in
    let let_acc next =
      [%expr
        let [%p pvar acc_name] = Errors.empty () in
        [%e next]]
    in
    ((fun next -> let_loc @@ let_acc @@ next), loc_name, acc_name)

  let mk_pre_checks acc_name fun_name eloc pres =
    let pre_epxr = pre acc_name fun_name eloc pres in
    fun next ->
      pexp_sequence pre_epxr @@ pexp_sequence (F.report_pre acc_name) @@ next

  let mk_call acc_name ret_pat loc fun_name eloc xpost eargs =
    let call = pexp_apply (evar fun_name) eargs in
    let check_raises = xpost_guard acc_name loc fun_name eloc xpost call in
    fun next ->
      [%expr
        let [%p ret_pat] = [%e check_raises] in
        [%e next]]

  let mk_post_checks acc_name fun_name eloc terms next =
    let post_expr = post acc_name fun_name eloc terms in
    pexp_sequence post_expr (pexp_sequence (F.report_post acc_name) next)
end

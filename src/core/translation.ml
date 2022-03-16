module T = Types
open Ppxlib
open Gospel
open Fmt
open Builder
open Translated
module F = Failure
module Ident = Identifier.Ident

let rec pattern pn =
  let pattern' (p : Tterm.pattern) = pattern p.p_node in
  match pn with
  | Tterm.Pwild -> ppat_any
  | Tterm.Pvar v -> pvar (str "%a" Ident.pp v.vs_name)
  | Tterm.Papp (l, pl) when Tterm.is_fs_tuple l ->
      ppat_tuple (List.map pattern' pl)
  | Tterm.Papp (l, pl) ->
      let args =
        if pl = [] then None else Some (ppat_tuple (List.map pattern' pl))
      in
      ppat_construct (lident l.ls_name.id_str) args
  | Tterm.Por (p1, p2) -> ppat_or (pattern' p1) (pattern' p2)
  | Tterm.Pas (p, v) ->
      ppat_alias (pattern' p) (noloc (str "%a" Ident.pp v.vs_name))

type bound = Inf of expression | Sup of expression

let rec bounds ~driver ~loc (var : Tterm.vsymbol) (t1 : Tterm.term)
    (t2 : Tterm.term) =
  let unsupported () =
    raise (W.Error (Unsupported "ill formed quantification", loc))
  in
  (* [comb] extracts a bound from an the operator [f] and expression [e].
     [right] indicates if [e] is on the right side of the operator. *)
  let comb ~right (f : Tterm.lsymbol) e =
    match f.ls_name.id_str with
    | "infix >=" -> if right then Inf e else Sup e
    | "infix <=" -> if right then Sup e else Inf e
    | "infix <" -> if right then Sup (epred e) else Inf (esucc e)
    | "infix >" -> if right then Inf (esucc e) else Sup (epred e)
    | _ -> unsupported ()
  in
  let bound = function
    | Tterm.Tapp (f, [ { t_node = Tvar vs; _ }; t ])
      when vs.vs_name = var.vs_name ->
        comb ~right:true f (unsafe_term ~driver t)
    | Tterm.Tapp (f, [ t; { t_node = Tvar vs; _ } ])
      when vs.vs_name = var.vs_name ->
        comb ~right:false f (unsafe_term ~driver t)
    | _ -> unsupported ()
  in
  match (bound t1.t_node, bound t2.t_node) with
  | Inf start, Sup stop | Sup stop, Inf start -> (start, stop)
  | _ -> unsupported ()

and unsafe_term ~driver (t : Tterm.term) : expression =
  let term = unsafe_term ~driver in
  let loc = t.t_loc in
  let unsupported m = raise (W.Error (W.Unsupported m, loc)) in
  match t.t_node with
  | Tvar { vs_name; _ } -> evar (str "%a" Ident.pp vs_name)
  | Tconst c -> econst c
  | Tfield (t, f) -> pexp_field (term t) (lident f.ls_name.id_str)
  | Tapp (fs, []) when Tterm.(ls_equal fs fs_bool_true) -> [%expr true]
  | Tapp (fs, []) when Tterm.(ls_equal fs fs_bool_false) -> [%expr false]
  | Tapp (fs, tlist) when Tterm.is_fs_tuple fs ->
      List.map term tlist |> pexp_tuple
  | Tapp (ls, tlist) when Drv.is_function ls driver ->
      let f = Drv.find_function ls driver in
      eapply (evar f) (List.map term tlist)
  | Tapp (ls, tlist) when Tterm.(ls_equal ls fs_apply) ->
      let f, args =
        match tlist with
        | [] -> assert false
        | x :: xs -> (term x, List.map term xs)
      in
      eapply f args
  | Tapp (ls, [ lhs; rhs ]) when Tterm.(ls_equal ls ps_equ) ->
      let eq = T.Equality.derive ~loc driver lhs.t_ty in
      eapply eq [ term lhs; term rhs ]
  | Tapp (ls, tlist) -> (
      Drv.translate_stdlib ls driver |> function
      | Some f -> eapply (evar f) (List.map term tlist)
      | None ->
          let func = ls.ls_name.id_str in
          if ls.ls_constr then
            (if tlist = [] then None
            else Some (List.map term tlist |> pexp_tuple))
            |> pexp_construct (lident func)
          else kstr unsupported "function application `%s`" func)
  | Tif (i, t, e) -> [%expr if [%e term i] then [%e term t] else [%e term e]]
  | Tlet (x, t1, t2) ->
      let x = str "%a" Ident.pp x.vs_name in
      [%expr
        let [%p pvar x] = [%e term t1] in
        [%e term t2]]
  | Tcase (t, ptl) ->
      List.map
        (fun (p, t) ->
          case ~guard:None ~lhs:(pattern p.Tterm.p_node) ~rhs:(term t))
        ptl
      |> pexp_match (term t)
  | Tquant (Tterm.Tlambda, args, t) ->
      let t = term t in
      let args =
        List.map
          (fun (vs : Tterm.vsymbol) ->
            (Nolabel, pvar (str "%a" Ident.pp vs.vs_name)))
          args
      in
      efun args t
  | Tquant
      ( (Tterm.(Tforall | Texists) as quant),
        [ var ],
        Tterm.
          {
            t_node =
              Tbinop
                ( ((Timplies | Tand | Tand_asym) as op),
                  { t_node = Tbinop (Tand, t1, t2); _ },
                  p );
            _;
          } ) ->
      (match (quant, op) with
      | Tforall, Timplies | Texists, (Tand | Tand_asym) -> ()
      | _, _ -> unsupported "ill formed quantification");
      let start, stop = bounds ~driver ~loc var t1 t2 in
      let p = term p in
      let quant =
        evar
          (if quant = Tforall then "Ortac_runtime.Z.forall"
          else "Ortac_runtime.Z.exists")
      in
      let x = str "%a" Ident.pp var.vs_name in
      let func = pexp_fun Nolabel None (pvar x) p in
      eapply quant [ start; stop; func ]
  | Tquant (_, _, _) -> unsupported "quantification"
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

let term ~driver fail t =
  try
    Ok
      [%expr
        try [%e unsafe_term ~driver t]
        with e ->
          [%e fail (evar "e")];
          true]
  with W.Error t -> Error t

let conditions ~driver ~term_printer fail_violated fail_nonexec terms =
  List.map
    (fun t ->
      let txt = term_printer t in
      let loc = t.Tterm.t_loc in
      let translation =
        term ~driver (fail_nonexec txt) t
        |> Result.map (fun t ->
               [%expr if not [%e t] then [%e fail_violated txt]])
      in
      ({ txt; loc; translation } : term))
    terms

let with_models ~driver:_ fields (type_ : type_) =
  let models =
    List.map (fun ((ls : Tterm.lsymbol), b) -> (ls.ls_name.id_str, b)) fields
  in
  { type_ with models }

let subst_invariant_fields var (t : Tterm.term) =
  let rec aux t =
    match t.Tterm.t_node with
    | Tapp (ls, []) when ls.ls_field ->
        { t with t_node = Tterm.Tfield (var, ls) }
    | Tvar _ | Tconst _ | Ttrue | Tfalse -> t
    | Tapp (ls, tl) ->
        let tl = List.map aux tl in
        let t_node = Tterm.Tapp (ls, tl) in
        { t with t_node }
    | Tfield (t, ls) ->
        let t = aux t in
        let t_node = Tterm.Tfield (t, ls) in
        { t with t_node }
    | Tif (t1, t2, t3) ->
        let t1 = aux t1 in
        let t2 = aux t2 in
        let t3 = aux t3 in
        let t_node = Tterm.Tif (t1, t2, t3) in
        { t with t_node }
    | Tterm.Tlet (vs, t1, t2) ->
        let t1 = aux t1 in
        let t2 = aux t2 in
        let t_node = Tterm.Tlet (vs, t1, t2) in
        { t with t_node }
    | Tterm.Tcase (t, ptl) ->
        let t = aux t in
        let ptl = List.map (fun (p, t) -> (p, aux t)) ptl in
        let t_node = Tterm.Tcase (t, ptl) in
        { t with t_node }
    | Tquant (q, vsl, t) ->
        let t = aux t in
        let t_node = Tterm.Tquant (q, vsl, t) in
        { t with t_node }
    | Tterm.Tbinop (op, t1, t2) ->
        let t1 = aux t1 in
        let t2 = aux t2 in
        let t_node = Tterm.Tbinop (op, t1, t2) in
        { t with t_node }
    | Tterm.Tnot t ->
        let t = aux t in
        let t_node = Tterm.Tnot t in
        { t with t_node }
    | Tterm.Told t ->
        let t = aux t in
        let t_node = Tterm.Told t in
        { t with t_node }
  in
  aux t

let invariant ~driver ~term_printer (invariant : Tterm.term) =
  let function_name = gen_symbol ~prefix:"__invariant_" () in
  let instance_id = Ident.create ~loc (gen_symbol ~prefix:"__self_" ()) in
  let instance_arg = (Nolabel, pvar (Fmt.str "%a" Ident.pp instance_id)) in
  let instance_term =
    (* XXX This is not the correct type or location, but it doesn't matter for
       the translation *)
    Tterm.t_var { vs_name = instance_id; vs_ty = Ttypes.ty_unit } loc
  in

  let register_name = gen_symbol ~prefix:"__error_" () in
  let register_name_arg = (Nolabel, pvar register_name) in
  let register_name = evar register_name in

  let position = gen_symbol ~prefix:"__position_" () in
  let eposition = evar position in
  let position_arg = (Nolabel, pvar position) in

  let violated term = F.violated_invariant eposition ~term ~register_name in
  let nonexec term exn =
    F.invariant_failure eposition ~term ~exn ~register_name
  in
  let txt = term_printer invariant in
  let loc = invariant.Tterm.t_loc in
  let translation =
    let invariant = subst_invariant_fields instance_term invariant in
    term ~driver (nonexec txt) invariant
    |> Result.map (fun e -> [%expr if not [%e e] then [%e violated txt]])
    |> Result.map (efun [ register_name_arg; position_arg; instance_arg ])
    |> Result.map (fun e ->
           (function_name, [%stri let [%p pvar function_name] = [%e e]]))
  in
  { txt; loc; translation }

let with_invariants ~driver ~term_printer invariants (type_ : type_) =
  let invariants = List.map (invariant ~driver ~term_printer) invariants in
  { type_ with invariants }

let with_consumes consumes (value : value) =
  let name (t : Tterm.term) =
    match t.t_node with
    | Tterm.Tvar vs -> Some (Fmt.str "%a" Tast.Ident.pp vs.vs_name)
    | _ -> None
  in
  let consumes = List.filter_map name consumes in
  let arguments =
    List.map (* not very efficient *)
      (fun (a : Translated.ocaml_var) ->
        if List.exists (fun c -> a.name = c) consumes then
          { a with consumed = true }
        else a)
      value.arguments
  in
  { value with arguments }

let with_modified modifies (value : value) =
  let name (t : Tterm.term) =
    match t.t_node with
    | Tterm.Tvar vs -> Some (Fmt.str "%a" Tast.Ident.pp vs.vs_name)
    | _ -> None
  in
  let modifies = List.filter_map name modifies in
  let arguments =
    List.map (* not very efficient *)
      (fun (a : Translated.ocaml_var) ->
        if List.exists (fun c -> a.name = c) modifies then
          { a with modified = true }
        else a)
      value.arguments
  in
  { value with arguments }

let with_pres ~driver ~term_printer pres (value : value) =
  let register_name = evar value.register_name in
  let violated term = F.violated `Pre ~term ~register_name in
  let nonexec term exn = F.spec_failure `Pre ~term ~exn ~register_name in
  let preconditions = conditions ~driver ~term_printer violated nonexec pres in
  { value with preconditions }

let with_checks ~driver ~term_printer checks (value : value) =
  let register_name = evar value.register_name in
  let nonexec term exn = F.spec_failure `Check ~term ~exn ~register_name in
  let checks =
    List.map
      (fun t ->
        let txt = term_printer t in
        let loc = t.Tterm.t_loc in
        let term = term ~driver (nonexec txt) t in
        let translations =
          Result.map
            (fun t ->
              ( [%expr
                  if not [%e t] then
                    [%e F.uncaught_checks ~register_name ~term:txt]],
                [%expr if [%e t] then [%e F.unexpected_checks ~register_name]]
              ))
            term
        in
        { txt; loc; translations })
      checks
  in
  { value with checks }

let with_posts ~driver ~term_printer posts (value : value) =
  let register_name = evar value.register_name in
  let violated term = F.violated `Post ~term ~register_name in
  let nonexec term exn = F.spec_failure `Post ~term ~exn ~register_name in
  let postconditions =
    conditions ~driver ~term_printer violated nonexec posts
  in
  { value with postconditions }

let with_constant_checks ~driver ~term_printer checks (constant : constant) =
  let register_name = evar constant.register_name in
  let violated term = F.violated `Pre ~term ~register_name in
  let nonexec term exn = F.spec_failure `Pre ~term ~exn ~register_name in
  let checks = conditions ~driver ~term_printer violated nonexec checks in
  { constant with checks }

let rec xpost_pattern ~driver exn = function
  | Tterm.Papp (ls, []) when Tterm.(ls_equal ls (fs_tuple 0)) -> pvar exn
  | Tterm.Papp (ls, _l) when not (Tterm.is_fs_tuple ls) -> assert false
  | Tterm.Por (p1, p2) ->
      ppat_or
        (xpost_pattern ~driver exn p1.p_node)
        (xpost_pattern ~driver exn p2.p_node)
  | Tterm.Pas (p, s) ->
      ppat_alias
        (xpost_pattern ~driver exn p.p_node)
        (noloc (str "%a" Tterm.Ident.pp s.vs_name))
  | pn -> ppat_construct (lident exn) (Some (pattern pn))

let assert_false_case =
  case ~guard:None ~lhs:[%pat? _] ~rhs:[%expr assert false]

let with_xposts ~driver ~term_printer xposts (value : value) =
  let register_name = evar value.register_name in
  let xpost (exn, ptlist) =
    let name = exn.Ttypes.xs_ident.id_str in
    let cases =
      List.map
        (fun (p, t) ->
          let s = term_printer t in
          let nonexec exn = F.spec_failure `XPost ~term:s ~exn ~register_name in
          term ~driver nonexec t
          |> Result.map (fun t ->
                 case ~guard:None
                   ~lhs:(xpost_pattern ~driver name p.Tterm.p_node)
                   ~rhs:
                     [%expr
                       if not [%e t] then
                         [%e F.violated `XPost ~term:s ~register_name]]))
        (* XXX ptlist must be rev because the cases are given in the
           reverse order by gospel *)
        (List.rev ptlist)
    in
    if List.exists Result.is_error cases then
      List.filter_map (function Ok _ -> None | Error x -> Some x) cases
      |> Result.error
    else List.map Result.get_ok cases @ [ assert_false_case ] |> Result.ok
  in
  let xpostconditions =
    List.map
      (fun xp ->
        let xs = fst xp in
        let exn = xs.Ttypes.xs_ident.id_str in
        let args =
          match xs.Ttypes.xs_type with
          | Ttypes.Exn_tuple l -> List.length l
          | Ttypes.Exn_record _ -> 1
        in
        let translation = xpost xp in
        { exn; args; translation })
      xposts
  in
  { value with xpostconditions }

let function_definition ~driver ls i t : term =
  let txt = Fmt.str "%a" Tterm.print_term t in
  let loc = t.t_loc in
  let translation =
    let driver = Drv.add_function ls i driver in
    try Ok (unsafe_term ~driver t) with W.Error t -> Error t
  in
  { txt; loc; translation }

let axiom_definition ~driver ~register_name t : term =
  let register_name = evar register_name in
  let fail_violated = F.violated_axiom ~register_name in
  let fail_nonexec exn = F.axiom_failure ~exn ~register_name in
  let txt = Fmt.str "%a" Tterm.print_term t in
  let loc = t.t_loc in
  let translation =
    term ~driver fail_nonexec t
    |> Result.map (fun check ->
           [%expr
             if not [%e check] then [%e fail_violated];
             [%e F.report ~register_name]])
  in
  { txt; loc; translation }

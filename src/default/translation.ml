module W = Ortac_core.Warnings
open Ppxlib
open Gospel
open Fmt
open Ortac_core.Builder
open Translated
module F = Ortac_core.Failure
module Ident = Identifier.Ident

let term ~context fail t =
  try
    Ok
      [%expr
        try [%e Ortac_core.Ocaml_of_gospel.term ~context t]
        with e ->
          [%e fail (evar "e")];
          true]
  with W.Error t -> Error t

let conditions ~context ~term_printer fail_violated fail_nonexec terms =
  List.map
    (fun t ->
      let txt = term_printer t in
      let loc = t.Tterm.t_loc in
      let translation =
        term ~context (fail_nonexec txt) t
        |> Result.map (fun t ->
               [%expr if not [%e t] then [%e fail_violated txt]])
      in
      ({ txt; loc; translation } : term))
    terms

let with_models ~context:_ fields (type_ : type_) =
  let models =
    List.map (fun ((ls : Symbols.lsymbol), b) -> (ls.ls_name.id_str, b)) fields
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
        let ptl =
          List.map (fun (p, g, t) -> (p, Option.map aux g, aux t)) ptl
        in
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

let invariant ~context ~term_printer self (invariant : Tterm.term) =
  let function_name = gen_symbol ~prefix:"__invariant_" () in
  let instance_id =
    match self with
    | None -> Ident.create ~loc (gen_symbol ~prefix:"__self_" ())
    | Some self -> self.Symbols.vs_name
  in
  let instance_arg = (Nolabel, pvar (Fmt.str "%a" Ident.pp instance_id)) in
  let instance_term =
    (* XXX This is not the correct type or location, but it doesn't matter for
       the translation *)
    Tterm_helper.t_var { vs_name = instance_id; vs_ty = Ttypes.ty_unit } loc
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
    term ~context (nonexec txt) invariant
    |> Result.map (fun e -> [%expr if not [%e e] then [%e violated txt]])
    |> Result.map (efun [ register_name_arg; position_arg; instance_arg ])
    |> Result.map (fun e ->
           (function_name, [%stri let [%p pvar function_name] = [%e e]]))
  in
  { txt; loc; translation }

let with_invariants ~context ~term_printer (self, invariants) (type_ : type_) =
  let invariants = List.map (invariant ~context ~term_printer self) invariants in
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

let with_pres ~context ~term_printer pres (value : value) =
  let register_name = evar value.register_name in
  let violated term = F.violated `Pre ~term ~register_name in
  let nonexec term exn = F.spec_failure `Pre ~term ~exn ~register_name in
  let preconditions = conditions ~context ~term_printer violated nonexec pres in
  { value with preconditions }

let with_checks ~context ~term_printer checks (value : value) =
  let register_name = evar value.register_name in
  let nonexec term exn = F.spec_failure `Check ~term ~exn ~register_name in
  let checks =
    List.map
      (fun t ->
        let txt = term_printer t in
        let loc = t.Tterm.t_loc in
        let term = term ~context (nonexec txt) t in
        let check_id = gen_symbol ~prefix:"__check" () in
        let translations =
          Result.map
            (fun t ->
              ( (check_id, t),
                [%expr
                  if not [%e evar check_id] then
                    [%e F.uncaught_checks ~register_name ~term:txt]] ))
            term
        in
        { txt; loc; translations })
      checks
  in
  { value with checks }

let with_posts ~context ~term_printer posts (value : value) =
  let register_name = evar value.register_name in
  let violated term = F.violated `Post ~term ~register_name in
  let nonexec term exn = F.spec_failure `Post ~term ~exn ~register_name in
  let postconditions =
    conditions ~context ~term_printer violated nonexec posts
  in
  { value with postconditions }

let with_constant_checks ~context ~term_printer checks (constant : constant) =
  let register_name = evar constant.register_name in
  let violated term = F.violated `Pre ~term ~register_name in
  let nonexec term exn = F.spec_failure `Pre ~term ~exn ~register_name in
  let checks = conditions ~context ~term_printer violated nonexec checks in
  { constant with checks }

let rec xpost_pattern ~context exn = function
  | Tterm.Papp (ls, []) when Symbols.(ls_equal ls (fs_tuple 0)) -> pvar exn
  | Tterm.Papp (ls, _l) when not (Symbols.is_fs_tuple ls) -> assert false
  | Tterm.Por (p1, p2) ->
      ppat_or
        (xpost_pattern ~context exn p1.p_node)
        (xpost_pattern ~context exn p2.p_node)
  | Tterm.Pas (p, s) ->
      ppat_alias
        (xpost_pattern ~context exn p.p_node)
        (noloc (str "%a" Tterm.Ident.pp s.vs_name))
  | pn -> ppat_construct (lident exn) (Some (Ortac_core.Ocaml_of_gospel.pattern pn))

let assert_false_case =
  case ~guard:None ~lhs:[%pat? _] ~rhs:[%expr assert false]

let with_xposts ~context ~term_printer xposts (value : value) =
  let register_name = evar value.register_name in
  let xpost (exn, ptlist) =
    let name = exn.Ttypes.xs_ident.id_str in
    let cases =
      List.map
        (fun (p, t) ->
          let s = term_printer t in
          let nonexec exn = F.spec_failure `XPost ~term:s ~exn ~register_name in
          term ~context nonexec t
          |> Result.map (fun t ->
                 case ~guard:None
                   ~lhs:(xpost_pattern ~context name p.Tterm.p_node)
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

let function_definition ~context ls i t : term =
  let txt = Fmt.str "%a" Tterm_printer.print_term t in
  let loc = t.t_loc in
  let translation =
    let context = Ortac_core.Context.add_function ls i context in
    try Ok (Ortac_core.Ocaml_of_gospel.term ~context t) with W.Error t -> Error t
  in
  { txt; loc; translation }

let axiom_definition ~context ~register_name t : term =
  let register_name = evar register_name in
  let fail_violated = F.violated_axiom ~register_name in
  let fail_nonexec exn = F.axiom_failure ~exn ~register_name in
  let txt = Fmt.str "%a" Tterm_printer.print_term t in
  let loc = t.t_loc in
  let translation =
    term ~context fail_nonexec t
    |> Result.map (fun check ->
           [%expr
             if not [%e check] then [%e fail_violated];
             [%e F.report ~register_name]])
  in
  { txt; loc; translation }

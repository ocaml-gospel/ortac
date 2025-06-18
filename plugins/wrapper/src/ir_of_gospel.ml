open Gospel
module W = Ortac_core.Warnings
module F = Failure
open Fmt
open Ir
open Ppxlib
open Ortac_core.Builder
module Ident = Identifier.Ident
open Ortac_core.Utils
module VSet = Set.Make (Symbols.Vs)

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
    List.map
      (fun ((ls : Symbols.lsymbol), b) ->
        let name = ls.ls_name.id_str in
        let prefix = "__projection_" ^ name in
        let proj_name = gen_symbol ~prefix () in
        let ident = Ident.create ~loc:Location.none proj_name in
        (name, ident, b))
      fields
  in
  { type_ with models }

let find_projection ir field =
  let aux = function
    | Type t ->
        List.find_map
          (fun (name, ident, _) -> if field = name then Some ident else None)
          t.models
    | _ -> None
  in
  List.find_map aux ir.structure

let collect_model ir =
  (* replaces calls to the gospel model by the projection function *)
  let rec collect term =
    match term.Tterm.t_node with
    | Tvar _ | Tconst _ | Ttrue | Tfalse -> term
    | Tapp (ls, tl) ->
        let tl = List.map collect tl in
        let t_node = Tterm.Tapp (ls, tl) in
        { term with t_node }
    | Tfield (t, ls) ->
        let t = collect t in
        let field = ls.ls_name.id_str in
        let vs =
          let vs_ty = Option.value ls.ls_value ~default:Ttypes.ty_bool in
          Symbols.{ vs_name = ls.ls_name; vs_ty }
        in
        let proj = Tterm_helper.t_var vs Location.none in
        let t_node =
          if mem_model ir field then Tterm.Tapp (Symbols.fs_apply, [ proj; t ])
          else Tterm.Tfield (t, ls)
        in
        { term with t_node }
    | Tif (t1, t2, t3) ->
        let t1 = collect t1 in
        let t2 = collect t2 in
        let t3 = collect t3 in
        let t_node = Tterm.Tif (t1, t2, t3) in
        { term with t_node }
    | Tlet (vs, t1, t2) ->
        let t1 = collect t1 in
        let t2 = collect t2 in
        let t_node = Tterm.Tlet (vs, t1, t2) in
        { term with t_node }
    | Tcase (t, ptl) ->
        let t = collect t in
        let ptl =
          List.map (fun (p, g, t) -> (p, Option.map collect g, collect t)) ptl
        in
        let t_node = Tterm.Tcase (t, ptl) in
        { term with t_node }
    | Tlambda (pl, t) ->
        let t = collect t in
        let t_node = Tterm.Tlambda (pl, t) in
        { term with t_node }
    | Tquant (q, vsl, t) ->
        let t = collect t in
        let t_node = Tterm.Tquant (q, vsl, t) in
        { term with t_node }
    | Tbinop (op, t1, t2) ->
        let t1 = collect t1 in
        let t2 = collect t2 in
        let t_node = Tterm.Tbinop (op, t1, t2) in
        { term with t_node }
    | Tnot t ->
        let t = collect t in
        let t_node = Tterm.Tnot t in
        { term with t_node }
    | Told t -> collect t
  in
  collect

let subst_invariant_fields var (t : Tterm.term) =
  let rec aux t =
    match t.Tterm.t_node with
    | Tapp (ls, []) when ls.ls_field -> { t with t_node = Tfield (var, ls) }
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
    | Tlet (vs, t1, t2) ->
        let t1 = aux t1 in
        let t2 = aux t2 in
        let t_node = Tterm.Tlet (vs, t1, t2) in
        { t with t_node }
    | Tcase (t, ptl) ->
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
    | Tlambda (ps, t) ->
        let t = aux t in
        let t_node = Tterm.Tlambda (ps, t) in
        { t with t_node }
    | Tbinop (op, t1, t2) ->
        let t1 = aux t1 in
        let t2 = aux t2 in
        let t_node = Tterm.Tbinop (op, t1, t2) in
        { t with t_node }
    | Tnot t ->
        let t = aux t in
        let t_node = Tterm.Tnot t in
        { t with t_node }
    | Told t ->
        let t = aux t in
        let t_node = Tterm.Told t in
        { t with t_node }
  in
  aux t

let invariant ~context ~term_printer self ir (invariant : Tterm.term) =
  let function_name = gen_symbol ~prefix:"__invariant_" () in
  let instance_arg =
    (Nolabel, pvar (Fmt.str "%a" Ident.pp self.Symbols.vs_name))
  in
  let instance_term =
    (* XXX This is not the correct type or location, but it doesn't matter for
       the translation *)
    Tterm_helper.t_var
      { vs_name = self.Symbols.vs_name; vs_ty = Ttypes.ty_unit }
      loc
  in
  let invariant = collect_model ir invariant in

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

let with_invariants ~context ~term_printer ir invariants (type_ : type_) =
  Option.fold ~none:type_
    ~some:(fun (self, invariants) ->
      let invariants =
        List.map (invariant ~context ~term_printer self ir) invariants
      in
      { type_ with invariants })
    invariants

let with_consumes consumes (value : value) =
  let name (t : Tterm.term) =
    match t.t_node with
    | Tterm.Tvar vs -> Some (Fmt.str "%a" Tast.Ident.pp vs.vs_name)
    | _ -> None
  in
  let consumes = List.filter_map name consumes in
  let arguments =
    List.map (* not very efficient *)
      (fun (a : Ir.ocaml_var) ->
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
      (fun (a : Ir.ocaml_var) ->
        if List.exists (fun c -> a.name = c) modifies then
          { a with modified = true }
        else a)
      value.arguments
  in
  { value with arguments }

let with_pres ~context ~term_printer ir pres (value : value) =
  let register_name = evar value.register_name in
  let violated term = F.violated_condition `Pre ~term ~register_name in
  let nonexec term exn = F.spec_failure `Pre ~term ~exn ~register_name in
  let pres = List.map (collect_model ir) pres in
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

let rec get_pattern_vars p =
  match p.Tterm.p_node with
  | Pvar vs -> VSet.singleton vs
  | Papp (_, args) ->
      List.fold_left
        (fun acc p -> VSet.union acc (get_pattern_vars p))
        VSet.empty args
  | Por (p1, p2) -> VSet.union (get_pattern_vars p1) (get_pattern_vars p2)
  | Pas (p, s) -> VSet.add s (get_pattern_vars p)
  | Pwild | Pinterval _ | Pconst _ -> VSet.empty

(* insert 'old' operator for free variables *)
let rec old_vars b term : Tterm.term =
  match term.Tterm.t_node with
  | Tvar x when VSet.mem x b -> term
  | Tvar _ -> { term with t_node = Told term }
  | Tconst _ | Ttrue | Tfalse -> term
  | Tapp (ls, tl) ->
      let tl = List.map (old_vars b) tl in
      let t_node = Tterm.Tapp (ls, tl) in
      { term with t_node }
  | Tfield (t, ls) ->
      let t = old_vars b t in
      let t_node = Tterm.Tfield (t, ls) in
      { term with t_node }
  | Tif (t1, t2, t3) ->
      let t1 = old_vars b t1 in
      let t2 = old_vars b t2 in
      let t3 = old_vars b t3 in
      let t_node = Tterm.Tif (t1, t2, t3) in
      { term with t_node }
  | Tlet (vs, t1, t2) ->
      let t1 = old_vars b t1 in
      let t2 = old_vars (VSet.add vs b) t2 in
      let t_node = Tterm.Tlet (vs, t1, t2) in
      { term with t_node }
  | Tcase (t, ptl) ->
      let t = old_vars b t in
      let ptl =
        List.map
          (fun (p, g, t) ->
            ( p,
              Option.map (old_vars b) g,
              old_vars (VSet.union (get_pattern_vars p) b) t ))
          ptl
      in
      let t_node = Tterm.Tcase (t, ptl) in
      { term with t_node }
  | Tlambda (pl, t) ->
      let t =
        let b =
          List.fold_left (fun set p -> VSet.union set (get_pattern_vars p)) b pl
        in
        old_vars b t
      in
      let t_node = Tterm.Tlambda (pl, t) in
      { term with t_node }
  | Tquant (q, vsl, t) ->
      let t = old_vars (VSet.union (VSet.of_list vsl) b) t in
      let t_node = Tterm.Tquant (q, vsl, t) in
      { term with t_node }
  | Tbinop (op, t1, t2) ->
      let t1 = old_vars b t1 in
      let t2 = old_vars b t2 in
      let t_node = Tterm.Tbinop (op, t1, t2) in
      { term with t_node }
  | Tnot t ->
      let t = old_vars b t in
      let t_node = Tterm.Tnot t in
      { term with t_node }
  | Told t -> old_vars b t

(* Push the 'old' operator down the term tree. The introduction of 'old' is
   propagated until a base term is reached *)
let rec old_down b term : Tterm.term =
  match term.Tterm.t_node with
  | Tvar _ | Tconst _ | Ttrue | Tfalse -> term
  | Tapp (ls, tl) ->
      let tl = List.map (old_down b) tl in
      let t_node = Tterm.Tapp (ls, tl) in
      { term with t_node }
  | Tfield (t, ls) ->
      let t = old_down b t in
      let t_node = Tterm.Tfield (t, ls) in
      { term with t_node }
  | Tif (t1, t2, t3) ->
      let t1 = old_down b t1 in
      let t2 = old_down b t2 in
      let t3 = old_down b t3 in
      let t_node = Tterm.Tif (t1, t2, t3) in
      { term with t_node }
  | Tlet (vs, t1, t2) ->
      let t1 = old_down b t1 in
      let t2 = old_down (VSet.add vs b) t2 in
      let t_node = Tterm.Tlet (vs, t1, t2) in
      { term with t_node }
  | Tcase (t, ptl) ->
      let t = old_down b t in
      let ptl =
        List.map
          (fun (p, g, t) ->
            ( p,
              Option.map (old_down b) g,
              old_down (VSet.union (get_pattern_vars p) b) t ))
          ptl
      in
      let t_node = Tterm.Tcase (t, ptl) in
      { term with t_node }
  | Tlambda (pl, t) ->
      let t =
        let b =
          List.fold_left (fun set p -> VSet.union set (get_pattern_vars p)) b pl
        in
        old_down b t
      in
      let t_node = Tterm.Tlambda (pl, t) in
      { term with t_node }
  | Tquant (q, vsl, t) ->
      let t = old_down (VSet.union (VSet.of_list vsl) b) t in
      let t_node = Tterm.Tquant (q, vsl, t) in
      { term with t_node }
  | Tbinop (op, t1, t2) ->
      let t1 = old_down b t1 in
      let t2 = old_down b t2 in
      let t_node = Tterm.Tbinop (op, t1, t2) in
      { term with t_node }
  | Tnot t ->
      let t = old_down b t in
      let t_node = Tterm.Tnot t in
      { term with t_node }
  | Told t -> old_vars b t

let fresh_var =
  let id = ref 0 in
  fun ty ->
    incr id;
    let str = Fmt.str "___ortac_copy_%d" !id in
    let preid = Identifier.Preid.create ~loc:Location.none str in
    Symbols.create_vsymbol preid (Option.value ~default:Ttypes.ty_bool ty)

let collect_old t =
  (* replaces terms under `old` operator with a variable and collect these
     variables in the [acc] *)
  let rec aux acc term =
    match term.Tterm.t_node with
    | Tvar _ | Tconst _ | Ttrue | Tfalse -> (acc, term)
    | Tapp (ls, tl) ->
        let acc, tl = List.fold_left_map aux acc tl in
        let t_node = Tterm.Tapp (ls, tl) in
        (acc, { term with t_node })
    | Tfield (t, ls) ->
        let acc, t = aux acc t in
        let t_node = Tterm.Tfield (t, ls) in
        (acc, { term with t_node })
    | Tif (t1, t2, t3) ->
        let acc, t1 = aux acc t1 in
        let acc, t2 = aux acc t2 in
        let acc, t3 = aux acc t3 in
        let t_node = Tterm.Tif (t1, t2, t3) in
        (acc, { term with t_node })
    | Tlet (vs, t1, t2) ->
        let acc, t1 = aux acc t1 in
        let acc, t2 = aux acc t2 in
        let t_node = Tterm.Tlet (vs, t1, t2) in
        (acc, { term with t_node })
    | Tcase (t, ptl) ->
        let acc, t = aux acc t in
        let acc, ptl =
          List.fold_left_map
            (fun acc (p, (g : Tterm.term option), t) ->
              let acc, g =
                Option.fold ~none:(acc, None)
                  ~some:(fun g ->
                    let acc, g = aux acc g in
                    (acc, Some g))
                  g
              in
              let acc, t = aux acc t in
              (acc, (p, g, t)))
            acc ptl
        in
        let t_node = Tterm.Tcase (t, ptl) in
        (acc, { term with t_node })
    | Tlambda (pl, t) ->
        let acc, t = aux acc t in
        let t_node = Tterm.Tlambda (pl, t) in
        (acc, { term with t_node })
    | Tquant (q, vsl, t) ->
        let acc, t = aux acc t in
        let t_node = Tterm.Tquant (q, vsl, t) in
        (acc, { term with t_node })
    | Tbinop (op, t1, t2) ->
        let acc, t1 = aux acc t1 in
        let acc, t2 = aux acc t2 in
        let t_node = Tterm.Tbinop (op, t1, t2) in
        (acc, { term with t_node })
    | Tnot t ->
        let acc, t = aux acc t in
        let t_node = Tterm.Tnot t in
        (acc, { term with t_node })
    | Told t ->
        let vs = fresh_var t.t_ty in
        let t_node = Tterm.Tvar vs in
        ((vs, t) :: acc, { term with t_node })
  in
  aux [] t

let with_posts ~context ~term_printer ir posts (value : value) =
  let register_name = evar value.register_name in
  let violated term = F.violated_condition `Post ~term ~register_name in
  let nonexec term exn = F.spec_failure `Post ~term ~exn ~register_name in
  let copies, posts =
    List.fold_left_map
      (fun acc t ->
        let copies, t' = collect_old (old_down VSet.empty t) in
        (copies @ acc, t'))
      [] posts
  in
  let copies =
    List.map
      (fun (vs, t) ->
        ( str "%a" Tterm.Ident.pp vs.Symbols.vs_name,
          Ortac_core.Ocaml_of_gospel.term ~context t ))
      copies
  in
  let posts = List.map (collect_model ir) posts in
  let postconditions =
    conditions ~context ~term_printer violated nonexec posts
  in
  { value with copies = copies @ value.copies; postconditions }

let with_constant_checks ~context ~term_printer checks (constant : Ir.constant)
    =
  let register_name = evar constant.register_name in
  let violated term = F.violated_condition `Pre ~term ~register_name in
  let nonexec term exn = F.spec_failure `Pre ~term ~exn ~register_name in
  let checks = conditions ~context ~term_printer violated nonexec checks in
  { constant with checks }

let rec xpost_pattern ~context exn p =
  match p.Tterm.p_node with
  | Papp (ls, []) when Symbols.(ls_equal ls (fs_tuple 0)) -> pvar exn
  | Papp (ls, _l) when not (Symbols.is_fs_tuple ls) -> assert false
  | Por (p1, p2) ->
      ppat_or (xpost_pattern ~context exn p1) (xpost_pattern ~context exn p2)
  | Pas (p, s) ->
      ppat_alias
        (xpost_pattern ~context exn p)
        (noloc (str "%a" Tterm.Ident.pp s.vs_name))
  | _ ->
      ppat_construct (lident exn) (Some (Ortac_core.Ocaml_of_gospel.pattern p))

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
                   ~lhs:(xpost_pattern ~context name p)
                   ~rhs:
                     [%expr
                       if not [%e t] then
                         [%e F.violated_condition `XPost ~term:s ~register_name]]))
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
    try Ok (Ortac_core.Ocaml_of_gospel.term ~context t)
    with W.Error t -> Error t
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

module P = struct
  type pack = { ir : Ir.t; context : Ortac_core.Context.t }

  let pack ir context = { ir; context }
  let unpack pack = (pack.ir, pack.context)
end

let register_name = gen_symbol ~prefix:"__error"

let type_of_ty ~ir (ty : Ttypes.ty) =
  match ty.ty_node with
  | Tyvar a ->
      Ir.type_ ~name:a.tv_name.id_str ~loc:a.tv_name.id_loc ~ghost:Tast.Nonghost
  | Tyapp (ts, _tvs) -> (
      match Ir.get_type ts ir with
      | None ->
          Ir.type_ ~name:ts.ts_ident.id_str ~loc:ts.ts_ident.id_loc
            ~ghost:Tast.Nonghost
      | Some type_ -> type_)

let vsname (vs : Symbols.vsymbol) = Fmt.str "%a" Tast.Ident.pp vs.vs_name

let var_of_vs ~ir (vs : Symbols.vsymbol) : Ir.ocaml_var =
  let name = vsname vs in
  let label = Nolabel in
  let type_ = type_of_ty ~ir vs.vs_ty in
  { name; label; type_; modified = false; consumed = false }

let var_of_arg ~ir arg : Ir.ocaml_var =
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
  let type_ = type_of_ty ~ir (Tast_helper.ty_of_lb_arg arg) in
  { name; label; type_; modified = false; consumed = false }

let type_ ~pack ~ghost (td : Tast.type_declaration) =
  let ir, context = P.unpack pack in
  let name = td.td_ts.ts_ident.id_str in
  let loc = td.td_loc in
  let type_ = Ir.type_ ~name ~loc ~ghost in
  let process ~type_ (spec : Tast.type_spec) =
    let term_printer = term_printer spec.ty_text spec.ty_loc in
    let type_ = with_models ~context spec.ty_fields type_ in
    let temp_item = Ir.Type type_ in
    let temp_ir =
      ir |> Ir.add_translation temp_item |> Ir.add_type td.td_ts type_
    in
    with_invariants ~context ~term_printer temp_ir spec.ty_invariants type_
  in
  let type_ = Option.fold ~none:type_ ~some:(process ~type_) td.td_spec in
  let type_item = Ir.Type type_ in
  let ir = ir |> Ir.add_translation type_item |> Ir.add_type td.td_ts type_ in
  P.pack ir context

let types ~pack ~ghost = List.fold_left (fun pack -> type_ ~pack ~ghost) pack

let is_projection (vd : Tast.val_description) =
  let aux = function
    | { attr_name = { txt = "model"; _ }; _ } -> true
    | _ -> false
  in
  List.exists aux vd.vd_attrs

let value ~pack ~ghost (vd : Tast.val_description) =
  let ir, context = P.unpack pack in
  let name = vd.vd_name.id_str in
  let loc = vd.vd_loc in
  let register_name = register_name () in
  let arguments = List.map (var_of_arg ~ir) vd.vd_args in
  let returns = List.map (var_of_arg ~ir) vd.vd_ret in
  let model = is_projection vd in
  let pure = false in
  let value =
    Ir.value ~name ~loc ~register_name ~arguments ~returns ~pure ~ghost ~model
  in
  let process ~value (spec : Tast.val_spec) =
    let term_printer = term_printer spec.sp_text spec.sp_loc in
    let value =
      value
      |> with_checks ~context ~term_printer spec.sp_checks
      |> with_pres ~context ~term_printer ir spec.sp_pre
      |> with_posts ~context ~term_printer ir spec.sp_post
      |> with_xposts ~context ~term_printer spec.sp_xpost
      |> with_consumes spec.sp_cs
      |> with_modified spec.sp_wr
    in
    { value with pure = spec.sp_pure }
  in
  let value = Option.fold ~none:value ~some:(process ~value) vd.vd_spec in
  let value_item = Ir.Value value in
  let context =
    if value.pure then
      let ls = Ortac_core.Context.get_ls context [ name ] in
      Ortac_core.Context.add_function ls name context
    else context
  in
  let ir = Ir.add_translation value_item ir in
  P.pack ir context

let constant ~pack ~ghost (vd : Tast.val_description) =
  let ir, context = P.unpack pack in
  let name = vd.vd_name.id_str in
  let loc = vd.vd_loc in
  let register_name = register_name () in
  let type_ =
    assert (List.length vd.vd_ret = 1);
    type_of_ty ~ir (Tast_helper.ty_of_lb_arg (List.hd vd.vd_ret))
  in
  let constant = Ir.constant ~name ~loc ~register_name ~type_ ~ghost in
  let process ~constant (spec : Tast.val_spec) =
    let term_printer = term_printer spec.sp_text spec.sp_loc in
    constant |> with_constant_checks ~context ~term_printer spec.sp_post
  in
  let c = Option.fold ~none:constant ~some:(process ~constant) vd.vd_spec in
  let ir = Ir.add_translation (Constant c) ir in
  P.pack ir context

let function_of (kind : [ `Function | `Predicate ]) ~pack (f : Tast.function_) =
  let ir, context = P.unpack pack in
  let name = gen_symbol ~prefix:("__logical_" ^ f.fun_ls.ls_name.id_str) () in
  let loc = f.fun_loc in
  let rec_ = f.fun_rec in
  let arguments = List.map (var_of_vs ~ir) f.fun_params in
  let definition =
    Option.map (function_definition ~context f.fun_ls name) f.fun_def
  in
  let translation =
    match kind with
    | `Function -> Ir.Function { name; loc; rec_; arguments; definition }
    | `Predicate -> Ir.Predicate { name; loc; rec_; arguments; definition }
  in
  let ir = Ir.add_translation translation ir in
  let context = Ortac_core.Context.add_function f.fun_ls name context in
  P.pack ir context

let function_ = function_of `Function
let predicate = function_of `Predicate

let axiom ~pack (ax : Tast.axiom) =
  let ir, context = P.unpack pack in
  let name = ax.ax_name.id_str in
  let loc = ax.ax_loc in
  let register_name = register_name () in
  let definition = axiom_definition ~context ~register_name ax.ax_term in
  let ir =
    Ir.add_translation (Axiom { name; loc; register_name; definition }) ir
  in
  P.pack ir context

let signature ~context s =
  let pack = P.pack (Ir.init context) context in
  List.fold_left
    (fun pack (sig_item : Tast.signature_item) ->
      match sig_item.sig_desc with
      | Sig_val (vd, ghost) when vd.vd_args <> [] -> value ~pack ~ghost vd
      | Sig_val (vd, ghost) -> constant ~pack ~ghost vd
      | Sig_type (_rec, td, ghost) -> types ~pack ~ghost td
      | Sig_function func when Option.is_none func.fun_ls.ls_value ->
          predicate ~pack func
      | Sig_function func -> function_ ~pack func
      | Sig_axiom ax -> axiom ~pack ax
      | _ -> pack)
    pack s
  |> P.unpack
  |> fst

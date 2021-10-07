module W = Warnings
open Translated

let term ppf (t : term) = Result.iter_error (W.pp ppf) t.translation
let terms ppf = List.iter (term ppf)

let invariants ppf =
  List.iter (fun (i : invariant) -> Result.iter_error (W.pp ppf) i.translation)

let xpost ppf (xp : xpost) =
  Result.iter_error (List.iter (W.pp ppf)) xp.translation

let xposts ppf = List.iter (xpost ppf)

let value ppf (v : value) =
  if v.ghost then W.pp ppf (W.Ghost_value v.name, v.loc)
  else (
    terms ppf v.preconditions;
    terms ppf v.postconditions;
    xposts ppf v.xpostconditions)

let type_ ppf (t : type_) =
  if t.ghost then W.pp ppf (W.Ghost_type t.name, t.loc)
  else (
    List.iter
      (fun (m, _) ->
        let t = (W.Unsupported_model (t.name, m), t.loc) in
        W.pp ppf t)
      t.models;
    (* Result.iter_error (W.pp ppf) t.equality; *)
    (* Result.iter_error (W.pp ppf) t.comparison; *)
    (* Result.iter_error (W.pp ppf) t.copy; *)
    invariants ppf t.invariants)

let constant ppf (c : constant) =
  if c.ghost then W.pp ppf (W.Ghost_value c.name, c.loc) else terms ppf c.checks

let definition ppf w loc = function
  | None -> W.pp ppf (w, loc)
  | Some def -> term ppf def

let function_ ppf (f : function_) =
  let w = W.Function_without_definition f.name in
  definition ppf w f.loc f.definition

let predicate ppf (p : function_) =
  let w = W.Predicate_without_definition p.name in
  definition ppf w p.loc p.definition

let axiom ppf (a : axiom) = term ppf a.definition

let emit_warnings ppf driver =
  Drv.iter_translation driver ~f:(function
    | Type t -> type_ ppf t
    | Value v -> value ppf v
    | Constant c -> constant ppf c
    | Function f -> function_ ppf f
    | Predicate p -> predicate ppf p
    | Axiom a -> axiom ppf a)

open Fmt
open Ppxlib

let pp_loc ppf loc =
  pf ppf "File \"%s\", line %d, characters %d-%d:" loc.loc_start.pos_fname
    loc.loc_start.pos_lnum
    (loc.loc_start.pos_cnum - loc.loc_start.pos_bol)
    (loc.loc_end.pos_cnum - loc.loc_start.pos_bol)

let translated_term ppf (term : Translated.term) =
  pf ppf "+ %s has been translated" term.txt

let not_translated_term ppf (term : Translated.term) =
  pf ppf "+ %s has not been translated" term.txt

let report_term ppf (term : Translated.term) =
  if Result.is_ok term.translation then translated_term ppf term
  else not_translated_term ppf term

let report_derived ppf = function
  | Error _ -> pf ppf "has not been derived"
  | Ok _ -> pf ppf "has been derived"

let report_terms = list ~sep:(any "@\n") report_term

let report_exn ppf xpost =
  if Result.is_ok xpost.translation then
    pf ppf "+ the clauses concerning the exception %s have been translated"
      xpost.exn
  else
    pf ppf "+ the clauses concerning the exception %s have not been translated"
      xpost.exn

let report_xposts = list ~sep:(any "@\n") report_exn

let report_argument ppf (argument : Translated.ocaml_var) =
  let cs = if argument.consumed then "is consumed" else "is not consumed" in
  pf ppf "%s %s@\n+ Invariants involved:@\n  @[%a@]" argument.name cs
    report_terms argument.type_.invariants

let report_arguments = list ~sep:(any "@\n") report_argument

let report_return ppf (argument : Translated.ocaml_var) =
  pf ppf "%s@\n+ Invariants involved:@\n  @[%a@]" argument.name report_terms
    argument.type_.invariants

let report_return_pattern = list ~sep:(any "@\n") report_return

let report_value ppf (value : Translated.value) =
  pf ppf
    "%a@\n\
     \the value %s:@\n\
    \ - Pure: %b@\n\
    \ - Ghost: %b@\n\
    \ - Preconditions:@\n\
    \  @[%a@]@\n\
     - Postconditions:@\n\
    \  @[%a@]@\n\
     - Xpostconditions:@\n\
    \  @[%a@]@\n\
     - Arguments:@\n\
    \  @[%a@]@\n\
     - Return:@\n\
    \  @[%a@]@\n"
    pp_loc value.loc value.name value.pure value.ghost report_terms
    value.preconditions report_terms value.postconditions report_xposts
    value.xpostconditions report_arguments value.arguments report_return_pattern
    value.returns

let report_values = list ~sep:(any "@\n") report_value

let report_constant ppf (constant : Translated.constant) =
  pf ppf
    "%a@\n\
     the constant value %s:@\n\
     - Ghost: %b@\n\
     - Checks:@\n\
    \  @[%a@]@\n\
     - Invariants: XXX TODO XXX" pp_loc constant.loc constant.name
    constant.ghost report_terms constant.checks

let report_constants = list ~sep:(any "@\n") report_constant

let report_type ppf (type_ : type_) =
  pf ppf
    "%a@\n\
    \ the type %s:@\n\
     - Mutable: %b@\n\
     - Ghost: %b@\n\
     - Invariants:@\n\
    \  @[%a@]@\n\
     - Equality: %a@\n\
     - Comparison: %a@\n\
     - Copy: %a@\n"
    pp_loc type_.loc type_.name type_.mutable_ type_.ghost report_terms
    type_.invariants report_derived type_.equality report_derived
    type_.comparison report_derived type_.copy

let report_types = list ~sep:(any "@\n") report_type

let report_function ppf s (function_ : function_) =
  let translation =
    match function_.definition with
    | None -> "has not been translated"
    | Some def ->
        Result.fold
          ~ok:(fun _ -> "has been translated")
          ~error:(fun _ -> "has not been translated")
          def.translation
  in
  pf ppf "%a@\nthe %s %s %s" pp_loc function_.loc s function_.name translation

let report_function_ ppf = report_function ppf "Gospel function"
let report_functions = list ~sep:(any "@\n") report_function_
let report_predicate ppf = report_function ppf "Gospel predicate"
let report_predicates = list ~sep:(any "@\n") report_predicate

let report_axiom ppf (axiom : axiom) =
  let translated =
    Result.fold
      ~ok:(fun _ -> "has been translated")
      ~error:(fun _ -> "has not been translated")
      axiom.definition.translation
  in
  pf ppf "%a@\nthe Gospel axiom %s %s" pp_loc axiom.loc axiom.name translated

let report_axioms = list ~sep:(any "@\n") report_axiom

type triage = {
  types : Translated.type_ list;
  values : Translated.value list;
  constants : Translated.constant list;
  functions : Translated.function_ list;
  predicates : Translated.function_ list;
  axioms : Translated.axiom list;
}

let empty =
  {
    types = [];
    values = [];
    constants = [];
    functions = [];
    predicates = [];
    axioms = [];
  }

let triage (item : Translated.structure_item) triage =
  match item with
  | Type t ->
      let types = t :: triage.types in
      { triage with types }
  | Value v ->
      let values = v :: triage.values in
      { triage with values }
  | Constant c ->
      let constants = c :: triage.constants in
      { triage with constants }
  | Function f ->
      let functions = f :: triage.functions in
      { triage with functions }
  | Predicate p ->
      let predicates = p :: triage.predicates in
      { triage with predicates }
  | Axiom a ->
      let axioms = a :: triage.axioms in
      { triage with axioms }

let report_module ppf (driver : Drv.t) =
  let triage = List.fold_right triage (Drv.translations driver) empty in
  pf ppf "%s@\n@[%a@]@\n@[%a@]@\n@[%a@]@\n@[%a@]@\n@[%a@]@\n@[%a@]"
    (Drv.module_name driver) report_types triage.types report_values
    triage.values report_constants triage.constants report_functions
    triage.functions report_predicates triage.predicates report_axioms
    triage.axioms

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

module Full_report = struct
  let pp_loc ppf loc =
    let open Ppxlib in
    pf ppf "%a" (styled `Bold Location.print) loc

  let quoted ppf s = pf ppf "`%s'" s

  let translation ppf = function
    | Error _ -> (styled `Yellow pf) ppf "has not been translated"
    | Ok _ -> (styled `Green pf) ppf "has been translated"

  let several_translations ppf = function
    | Error _ -> (styled `Yellow pf) ppf "have not been translated"
    | Ok _ -> (styled `Green pf) ppf "have been translated"

  let derived ppf = function
    | Error _ -> (styled `Yellow pf) ppf "has not been derived"
    | Ok _ -> (styled `Green pf) ppf "has been derived"

  let term ppf (term : Translated.term) =
    pf ppf "+ @[%a@\n%a %a@]" pp_loc term.loc quoted term.txt translation
      term.translation

  let terms = list ~sep:(any "@\n") term

  let invariant ppf (invariant : Translated.invariant) =
    pf ppf "+ @[%a@\n%a %a@]" pp_loc invariant.loc quoted invariant.txt
      translation invariant.translation

  let invariants = list ~sep:(any "@\n") invariant

  let exn ppf (xpost : xpost) =
    pf ppf "+ the clauses concerning the exception %s %a" xpost.exn
      several_translations xpost.translation

  let xposts = list ~sep:(any "@\n") exn

  let argument ppf (argument : Translated.ocaml_var) =
    let cs = if argument.consumed then "is consumed" else "is not consumed" in
    let md = if argument.modified then "is modified" else "is not modified" in
    pf ppf "%s %s and %s@\n+ Invariants involved:@\n  @[%a@]" argument.name cs
      md invariants argument.type_.invariants

  let arguments = list ~sep:(any "@\n") argument

  let return ppf (argument : Translated.ocaml_var) =
    pf ppf "%s@\n+ Invariants involved:@\n  @[%a@]" argument.name invariants
      argument.type_.invariants

  let return_pattern = list ~sep:(any "@\n") return

  let value ppf (value : Translated.value) =
    pf ppf
      "%a@\n\
       the value %s:@\n\
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
      pp_loc value.loc value.name value.pure value.ghost terms
      value.preconditions terms value.postconditions xposts
      value.xpostconditions arguments value.arguments return_pattern
      value.returns

  let constant ppf (constant : Translated.constant) =
    pf ppf
      "%a@\n\
       the constant value %s:@\n\
       - Ghost: %b@\n\
       - Checks:@\n\
      \  @[%a@]@\n\
       - Invariants:@\n\
      \  @[%a@]" pp_loc constant.loc constant.name constant.ghost terms
      constant.checks invariants constant.type_.invariants

  let type_ ppf (type_ : type_) =
    pf ppf
      "%a@\n\
       the type %s:@\n\
       - Mutable: %b@\n\
       - Ghost: %b@\n\
       - Invariants:@\n\
      \  @[%a@]@\n\
       - Equality: %a@\n\
       - Comparison: %a@\n\
       - Copy: %a@\n"
      pp_loc type_.loc type_.name type_.mutable_ type_.ghost invariants
      type_.invariants derived type_.equality derived type_.comparison derived
      type_.copy

  let function_ ppf s (function_ : function_) =
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

  let predicate ppf = function_ ppf "Gospel predicate"
  let function_ ppf = function_ ppf "Gospel function"

  let axiom ppf (axiom : axiom) =
    let translated =
      Result.fold
        ~ok:(fun _ -> "has been translated")
        ~error:(fun _ -> "has not been translated")
        axiom.definition.translation
    in
    pf ppf "%a@\nthe Gospel axiom %s %s" pp_loc axiom.loc axiom.name translated

  let triage ppf = function
    | Type t -> type_ ppf t
    | Value v -> value ppf v
    | Constant c -> constant ppf c
    | Function f -> function_ ppf f
    | Predicate p -> predicate ppf p
    | Axiom a -> axiom ppf a

  let module_ ppf driver = Drv.iter_translation ~f:(triage ppf) driver

  let report ppf (driver : Drv.t) =
    pf ppf "%s@\n@[%a@]" (Drv.module_name driver) module_ driver
end

let report = Full_report.report

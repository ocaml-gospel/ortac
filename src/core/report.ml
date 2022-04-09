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
  match v.ghost with
  | Gospel.Tast.Ghost -> W.pp ppf (W.Ghost_value v.name, v.loc)
  | Nonghost ->
      terms ppf v.preconditions;
      terms ppf v.postconditions;
      xposts ppf v.xpostconditions

let type_ ppf (t : type_) =
  match t.ghost with
  | Gospel.Tast.Ghost -> W.pp ppf (W.Ghost_type t.name, t.loc)
  | Nonghost ->
      List.iter
        (fun (m, _) ->
          let t = (W.Unsupported_model (t.name, m), t.loc) in
          W.pp ppf t)
        t.models;
      (* Result.iter_error (W.pp ppf) t.equality; *)
      (* Result.iter_error (W.pp ppf) t.comparison; *)
      (* Result.iter_error (W.pp ppf) t.copy; *)
      invariants ppf t.invariants

let constant ppf (c : constant) =
  match c.ghost with
  | Gospel.Tast.Ghost -> W.pp ppf (W.Ghost_value c.name, c.loc)
  | Nonghost -> terms ppf c.checks

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

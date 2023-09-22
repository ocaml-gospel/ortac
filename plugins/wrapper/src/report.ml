module W = Ortac_core.Warnings

type W.kind +=
  | Ghost_value of string
  | Ghost_type of string
  | Unsupported_model of string * string
  | Function_without_definition of string
  | Predicate_without_definition of string

let level = function
  | Ghost_value _ | Ghost_type _ | Unsupported_model _
  | Function_without_definition _ | Predicate_without_definition _ ->
      W.Warning
  | kind -> W.level kind

open Fmt

let pp_kind ppf = function
  | Ghost_value name ->
      pf ppf "%s is a ghost value. It was not translated." name
  | Ghost_type name -> pf ppf "%s is a ghost type. It was not translated." name
  | Unsupported_model (type_, name) ->
      pf ppf "Model %s of type %s is not supported. It was not translated." name
        type_
  | Function_without_definition name ->
      pf ppf "The function %s has no definition. It was not translated." name
  | Predicate_without_definition name ->
      pf ppf "The predicate %s has no definition. It was not translated." name
  | kind -> W.pp_kind ppf kind

let pp = W.pp_param pp_kind level

open Ir

let term ppf (t : term) = Result.iter_error (W.pp ppf) t.translation
let terms ppf = List.iter (term ppf)

let invariants ppf =
  List.iter (fun (i : invariant) -> Result.iter_error (W.pp ppf) i.translation)

let xpost ppf (xp : xpost) =
  Result.iter_error (List.iter (W.pp ppf)) xp.translation

let xposts ppf = List.iter (xpost ppf)

let value ppf (v : value) =
  match v.ghost with
  | Gospel.Tast.Ghost -> W.pp ppf (Ghost_value v.name, v.loc)
  | Nonghost ->
      terms ppf v.preconditions;
      terms ppf v.postconditions;
      xposts ppf v.xpostconditions

let type_ ppf (t : type_) =
  match t.ghost with
  | Gospel.Tast.Ghost -> pp ppf (Ghost_type t.name, t.loc)
  | Nonghost ->
      List.iter
        (fun (m, _) ->
          let t = (Unsupported_model (t.name, m), t.loc) in
          pp ppf t)
        t.models;
      (* Result.iter_error (W.pp ppf) t.equality; *)
      (* Result.iter_error (W.pp ppf) t.comparison; *)
      (* Result.iter_error (W.pp ppf) t.copy; *)
      invariants ppf t.invariants

let constant ppf (c : constant) =
  match c.ghost with
  | Gospel.Tast.Ghost -> pp ppf (Ghost_value c.name, c.loc)
  | Nonghost -> terms ppf c.checks

let definition ppf w loc = function
  | None -> pp ppf (w, loc)
  | Some def -> term ppf def

let function_ ppf (f : function_) =
  let w = Function_without_definition f.name in
  definition ppf w f.loc f.definition

let predicate ppf (p : function_) =
  let w = Predicate_without_definition p.name in
  definition ppf w p.loc p.definition

let axiom ppf (a : axiom) = term ppf a.definition

let emit_warnings ppf context =
  Ir.iter_translation context ~f:(function
    | Type t -> type_ ppf t
    | Value v -> value ppf v
    | Constant c -> constant ppf c
    | Function f -> function_ ppf f
    | Predicate p -> predicate ppf p
    | Axiom a -> axiom ppf a)

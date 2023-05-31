open Gospel
module Ident = Identifier.Ident

type xpost = Ttypes.xsymbol * (Tterm.pattern * Tterm.term) list

type next_state_formulae = {
  model : Ident.t; (* the name of the model's field *)
  description : Tterm.term; (* the new value for the model's field *)
}

type term = int * Tterm.term

(* XXX TODO decide whether we need checks here (if checks is false, state does
   not change) *)
type next_state = {
  (* description of the new values are stored with the index of the
     postcondition they come from *)
  formulae : (int * next_state_formulae) list;
  modifies : Ident.t list;
}

type postcond = {
  normal : term list;
  exceptional : xpost list;
  checks : Tterm.term list;
}

type value = {
  id : Ident.t;
  ty : Ppxlib.core_type;
  inst : (string * Ppxlib.core_type) list;
  sut_var : Ident.t;
  args : Ident.t option list; (* arguments of unit types are nameless *)
  next_state : next_state;
  postcond : postcond;
  precond : Tterm.term list;
}

let value id ty inst sut_var args next_state =
  {
    id;
    ty;
    inst;
    sut_var;
    args;
    next_state;
    postcond = { normal = []; exceptional = []; checks = [] };
    precond = [];
  }

type t = { state : (Ident.t * Ppxlib.core_type) list; values : value list }

let pp_state ppf state =
  let open Fmt in
  let pp_model ppf (id, ty) =
    pf ppf "@[%a: %a@]" Ident.pp id Ppxlib_ast.Pprintast.core_type ty
  in
  pf ppf "@[%a@]@." (list ~sep:(any "; ") pp_model) state

let pp_inst ppf inst =
  let open Fmt in
  let pp_binding ppf (v, t) =
    pf ppf "%a/%a" string v Ppxlib_ast.Pprintast.core_type t
  in
  pf ppf "[%a]" (list ~sep:(any ", ") pp_binding) inst

let pp_value ppf v =
  let open Fmt in
  pf ppf "id = %a; ty = %a; inst = %a@." Ident.pp v.id
    Ppxlib_ast.Pprintast.core_type v.ty pp_inst v.inst

let pp ppf t =
  let open Fmt in
  pf ppf "@[state = %a@]@[values = %a@]@." pp_state t.state (list pp_value)
    t.values

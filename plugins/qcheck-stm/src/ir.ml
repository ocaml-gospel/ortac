open Gospel
module Ident = Identifier.Ident

type xpost = Ttypes.xsymbol * Tterm.pattern option * Tterm.term

type new_state_formulae = {
  model : Ident.t; (* the name of the model's field *)
  description : Tterm.term; (* the new value for the model's field *)
}

type term = int * Tterm.term

(* XXX TODO decide whether we need checks here (if checks is false, state does
   not change) *)
type next_state = {
  (* description of the new values are stored with the index of the
     postcondition they come from *)
  formulae : (int * new_state_formulae) list;
  modifies : (Ident.t * Ppxlib.Location.t) list;
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
  args : (Ppxlib.core_type * Ident.t option) list;
      (* arguments of unit types can be nameless *)
  ret : Ident.t option;
  next_state : next_state;
  precond : Tterm.term list;
  postcond : postcond;
}

type init_state = {
  arguments : (Tast.lb_arg * Ppxlib.expression) list;
  descriptions : new_state_formulae list;
}

let get_return_type value =
  let open Ppxlib in
  let rec aux ty =
    match ty.ptyp_desc with Ptyp_arrow (_, _, r) -> aux r | _ -> ty
  in
  aux value.ty

let value id ty inst sut_var args ret next_state precond postcond =
  { id; ty; inst; sut_var; args; ret; next_state; precond; postcond }

type t = {
  state : (Ident.t * Ppxlib.core_type) list;
  init_state : init_state;
  ghost_functions : Tast.function_ list;
  values : value list;
}

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

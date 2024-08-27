open Gospel
module Ident = Identifier.Ident

type term = { term : Tterm.term; text : string }

let term ~prj_txt ~prj_loc spec term =
  let text = Ortac_core.Utils.term_printer (prj_txt spec) (prj_loc spec) term in
  { term; text }

let term_val =
  term ~prj_txt:(fun s -> s.Tast.sp_text) ~prj_loc:(fun s -> s.Tast.sp_loc)

let term_type =
  term ~prj_txt:(fun s -> s.Tast.ty_text) ~prj_loc:(fun s -> s.Tast.ty_loc)

type xpost = Ttypes.xsymbol * Tterm.pattern option * term

type new_state_formulae = {
  model : Ident.t; (* the name of the model's field *)
  description : Tterm.term; (* the new value for the model's field *)
}

type indexed_term = int * term

(* XXX TODO decide whether we need checks here (if checks is false, state does
   not change) *)
type next_state = {
  (* description of the new values are stored with the index of the
     postcondition they come from *)
  formulae : (int * new_state_formulae) list;
  modifies : (Ident.t * Ppxlib.Location.t) list;
}

type postcond = {
  normal : indexed_term list;
  exceptional : xpost list;
  checks : term list;
}

type value = {
  id : Ident.t;
  ty : Ppxlib.core_type;
  inst : (string * Ppxlib.core_type) list;
  sut_vars : Ident.t list;
      (* invariant: suts must be in the order in which they appear, so for
         example in [test (t1 : sut) (t2 : sut)] the list must be [t1; t2] *)
  args : (Ppxlib.core_type * Ident.t option) list;
      (* arguments of unit types can be nameless *)
  ret : Ident.t list;
  ret_values : term list list;
  next_states : (Ident.t * next_state) list;
      (* each used sut can have a different next state *)
  precond : Tterm.term list;
  postcond : postcond;
}

type init_state = {
  arguments : (Tast.lb_arg * Ppxlib.expression) list;
  returned_sut : Ident.t;
  descriptions : new_state_formulae list;
}

let get_return_type value =
  let open Ppxlib in
  let rec aux ty =
    match ty.ptyp_desc with Ptyp_arrow (_, _, r) -> aux r | _ -> ty
  in
  aux value.ty

let value id ty inst sut_vars args ret ret_values next_states precond postcond =
  {
    id;
    ty;
    inst;
    sut_vars;
    args;
    ret;
    ret_values;
    next_states;
    precond;
    postcond;
  }

type t = {
  state : (Ident.t * Ppxlib.core_type) list;
  invariants : (Ident.t * term list) option;
  init_state : init_state;
  ghost_functions : Tast.function_ list;
  ghost_types : (Tast.rec_flag * Tast.type_declaration list) list;
  values : value list;
}

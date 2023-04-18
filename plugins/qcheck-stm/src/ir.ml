open Gospel
module Ident = Identifier.Ident

type xpost = Ttypes.xsymbol * (Tterm.pattern * Tterm.term) list

type next_state = {
  formulae : Tterm.term list;
  modifies : Tterm.term list;
  checks : Tterm.term list;
}

type postcond = {
  normal : Tterm.term list;
  exceptional : xpost list;
  checks : Tterm.term list;
}

type value = {
  id : Ident.t;
  ty : Ppxlib.core_type;
  inst : (string * Ppxlib.core_type) list;
  sut_var : Ident.t;
  args : Ident.t list;
  next_state : next_state;
  postcond : postcond;
  precond : Tterm.term list;
}

let value id ty inst =
  {
    id;
    ty;
    inst;
    sut_var = Ident.create ~loc:Location.none "dummy_sut_var";
    args = [];
    next_state = { formulae = []; modifies = []; checks = [] };
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

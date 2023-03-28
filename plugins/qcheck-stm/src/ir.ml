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
  sut_var : Ident.t;
  args : Ident.t list;
  next_state : next_state;
  postcond : postcond;
  precond : Tterm.term list;
}

let value id =
  {
    id;
    ty = Ppxlib.Ast_helper.Typ.any ();
    sut_var = Ident.create ~loc:Location.none "dummy_sut_var";
    args = [];
    next_state = { formulae = []; modifies = []; checks = [] };
    postcond = { normal = []; exceptional = []; checks = [] };
    precond = [];
  }

module Pp = struct
  let value v = "id : " ^ v.id.Ident.id_str
end

type t = value list

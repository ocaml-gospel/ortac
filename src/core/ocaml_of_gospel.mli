val pattern : Gospel.Tterm.pattern_node -> Ppxlib.pattern
(** [pattern pn] translates a Gospel pattern into the matching OCaml pattern *)

val term : context:Context.t -> Gospel.Tterm.term -> Ppxlib.expression
(** [term ~context t] translates a Gospel typed term into the corresponding
    OCaml value using the translation [context] *)

val core_type_of_ty_with_subst :
  (string -> Ppxlib.core_type option) -> Gospel.Ttypes.ty -> Ppxlib.core_type
(** [core_type_of_ty_with_subst subst ty] translates a Gospel type into the
    corresponding OCaml [core_type] applying [subst] on the type variables *)

val core_type_of_tysymbol : Gospel.Ttypes.tysymbol -> Ppxlib.core_type
(** [core_type_of_tysymbol ts] translates a Gospel type symbol into the
    corresponding OCaml [core_type] **)

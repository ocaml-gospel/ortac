val pattern : Gospel.Tterm.pattern -> Ppxlib.pattern
(** [pattern p] translates a Gospel pattern into the matching OCaml pattern *)

val term : context:Context.t -> Gospel.Tterm.term -> Ppxlib.expression
(** [term ~context t] translates a Gospel typed term into the corresponding
    OCaml value using the translation [context] *)

val term_with_catch :
  context:Context.t -> Gospel.Tterm.term -> Ppxlib.expression
(** [term_with_catch ~context t] returns the expression generated by
    [term ~context t] inside a [try ... catch] that wraps any raised exception
    into an {!Ortac_runtime.Partial_function} exception to trace the origin of
    the exception *)

val term_with_catch_bool :
  context:Context.t -> Gospel.Tterm.term -> Ppxlib.expression
(** [term_with_catch_bool ~context t] returns the expression generated by
    [term ~context t] inside a [try ... catch] that returns [false] *)

val core_type_of_ty : context:Context.t -> Gospel.Ttypes.ty -> Ppxlib.core_type
(** [core_type_of_ty ~context ty] translates a Gospel type into the
    corresponding OCaml [core_type] *)

val core_type_of_ty_with_subst :
  context:Context.t ->
  (string -> Ppxlib.core_type option) ->
  Gospel.Ttypes.ty ->
  Ppxlib.core_type
(** [core_type_of_ty_with_subst subst ty] translates a Gospel type into the
    corresponding OCaml [core_type] applying [subst] on the type variables *)

val core_type_of_tysymbol :
  context:Context.t -> Gospel.Ttypes.tysymbol -> Ppxlib.core_type
(** [core_type_of_tysymbol ts] translates a Gospel type symbol into the
    corresponding OCaml [core_type] **)

val ocaml_type_decl_of_gospel_type_decl :
  context:Context.t -> Gospel.Tast.type_declaration -> Ppxlib.type_declaration
(** [ocaml_type_decl_of_gospel_type_decl ~context td] translates a Gospel type
    declaration into the corresponding OCaml [type_declaration]*)

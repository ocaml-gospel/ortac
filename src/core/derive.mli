type key
(** a [key] is a model of an object type *)

type expr
(** an [expr] contains the OCaml code needed to built a [ty Repr.t] where [ty]
    is the type corresponding to the [key] *)

type info
(** [info] contains the informations necessary to derive equality and comparison
    functions for a type corresponding to a [key] *)

type map
(** a [map] is a map from [key] to [info] *)

val to_string : key -> string
(** [to_string key] is a human friendly representation of the type corresponding
    to [key] for error reporting *)

val empty : map
(** [empty] is the empty [map] *)

val info : expr -> info
(** [info expr] is a smart constructor *)

val derive_all : map -> Ppxlib.structure
(** [derive_all map] derive all the [Repr.t], equality functions declaration
    necessary according to [map] *)

val add_info : key -> info -> map -> map
(** [add_info key info map] is [map] where [key] is now bound to [info]*)

val base : Ppxlib.expression -> expr
(** [base expr] is a smart constructor *)

val forall : int -> (Ppxlib.expression list -> Ppxlib.expression) -> expr
(** [forall i expr] is a smart constructor *)

val get_equality : key -> map -> string option
(** [get_equality key map] returns the name of the equality function in an
    [option] type *)

val add_equality : key -> map -> map
(** [add_equality key map] is [map] where the equality function for the type
    corresponding to [key] as a name *)

val traverse : Gospel.Tterm.term -> map -> map
(** [traverse term map] traverse recursively [term] updating accordingly [map],
    that is giving name to equality function when needed *)

val fold_traverse : Gospel.Tterm.term list -> map -> map
(** [fold_traverse terms map] traverse all the terms in [terms] updating
    accordingly [map] *)

val key : Gospel.Ttypes.ty -> key option
(** [key ty] is the [key] corresponding to the Gospel type [ty]. It returns
    [None] when [ty] is still paramettric *)

val key_from_tysymbol : Gospel.Ttypes.tysymbol -> key
(** [key_from_tysymbol ts] build the key corresponding to the Gospel type just
    containing [ts], that is [Tyapp (ts, \[\])]. This is necessary for the
    Gospelstdlib types in [Drv] *)

val raw_key : Gospel.Ttypes.ty -> key
(** [raw_key ty] is the [key] corresponding to the Gospel type [ty]. It is
    unsafe for [Repr.t] derivation as [ty] can be parametric. It is used for
    error reporting. *)

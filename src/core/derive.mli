type key
type map
type 'a kind = Base of 'a | Forall of int * ('a list -> 'a)
type expr = Ppxlib.expression kind
type info

val mbindings : map -> (key * info) list
val to_string : key -> string
val empty : map
val info : expr -> info
val expr : info -> expr
val eq : info -> string option
val add_info : key -> info -> map -> map
val base : Ppxlib.expression -> expr
val forall : int -> (Ppxlib.expression list -> Ppxlib.expression) -> expr
val get_repr : key -> map -> expr option
val get_equality : key -> map -> string option
val add_equality : key -> map -> map
val traverse : Gospel.Tterm.term -> map -> map
val fold_traverse : Gospel.Tterm.term list -> map -> map
val key : Gospel.Ttypes.ty -> key option
val key_from_tysymbol : Gospel.Ttypes.tysymbol -> key
val raw_key : Gospel.Ttypes.ty -> key

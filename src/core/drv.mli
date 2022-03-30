open Gospel

type t

val init : string -> Tmodule.namespace -> t
val module_name : t -> string
val translate_stdlib : Tterm.lsymbol -> t -> string option
val get_ls : t -> string list -> Tterm.lsymbol
val get_ts : t -> string list -> Ttypes.tysymbol
val get_type : Ttypes.tysymbol -> t -> Translated.type_ option
val add_type : Ttypes.tysymbol -> Translated.type_ -> t -> t
val is_function : Tterm.lsymbol -> t -> bool
val find_function : Tterm.lsymbol -> t -> string
val add_function : Tterm.lsymbol -> string -> t -> t
val add_translation : Translated.structure_item -> t -> t
val iter_translation : f:(Translated.structure_item -> unit) -> t -> unit
val map_translation : f:(Translated.structure_item -> 'a) -> t -> 'a list
val get_equality : t -> Ttypes.ty -> string option
val add_equality : Derive.key -> t -> t

(*XXX find a better name!!!*)
val repr : t -> Derive.map
val map_repr : f:(Derive.map -> Derive.map) -> t -> t

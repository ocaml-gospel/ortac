open Gospel

type t

val init : string -> Tmodule.namespace -> t
val module_name : t -> string
val translate_stdlib : Symbols.lsymbol -> t -> string option
val get_ls : t -> string list -> Symbols.lsymbol
val get_ts : t -> string list -> Ttypes.tysymbol
val get_type : Ttypes.tysymbol -> t -> Translated.type_ option
val add_type : Ttypes.tysymbol -> Translated.type_ -> t -> t
val is_function : Symbols.lsymbol -> t -> bool
val find_function : Symbols.lsymbol -> t -> string
val add_function : Symbols.lsymbol -> string -> t -> t
val add_translation : Translated.structure_item -> t -> t
val iter_translation : f:(Translated.structure_item -> unit) -> t -> unit
val map_translation : f:(Translated.structure_item -> 'a) -> t -> 'a list

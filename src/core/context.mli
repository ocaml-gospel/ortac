open Gospel_typecheckerlibs

type t
(** A value of type [t] contains the relevant information for translating Gospel
    typed terms into OCaml values. That is the Gospel standard library, Gospel
    namespace and symbols for the user defined logical functions. *)

val init : string -> Namespace.mod_defs -> t
(** [init module_name ns] builds the translation context for the module
    [module_name] according to the Gospel [namespace] *)

val module_name : t -> string
(** [module_name t] gets the name of the module currently being translated *)

val translate_stdlib : Id_uast.id -> t -> string option
(** [translate_stdlib ls t] finds the name of the OCaml function to call to
    translate [ls] if it is a symbol of the Gospel standard library or a
    built-in *)

val translate_tystdlib : Id_uast.id -> t -> string option
(** [translate_tystdlib ls t] finds the name of the OCaml type to use to
    translate [ts] if it is a type of the Gospel standard library or a built-in
*)

val get_ls : t -> string list -> Ident.t
(** [get_ls context qualid_name] gets the Gospel logical symbol corresponding to
    [qualid_name] in the [context] *)

val get_ts : t -> string list -> Ident.t
(** [get_ts context qualid_name] gets the Gospel type symbol corresponding to
    [qualid_name] in the [context] *)

val is_function : Id_uast.id -> t -> bool
(** [is_function ls context] returns [true] iff [ls] is the symbol of a function
    that has an implementation in [context] *)

val find_function : Id_uast.id -> t -> string
(** [find_function ls context] finds the name of the function to call to
    translate [ls] according to [context]. Raises [Not_found] if
    [is_function ls context] is false *)

val add_function : Id_uast.id -> string -> t -> t
(** [add_function ls name context] adds the mapping from [ls] to [name] in the
    functions stored in [context] *)

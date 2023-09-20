module W = Ortac_core.Warnings

type init_state_error =
  | Not_a_function_call of string
  | No_appropriate_specifications of string
  | Not_returning_sut of string
  | Qualified_name of string
  | Mismatch_number_of_arguments of string

type W.kind +=
  | Constant_value of string
  | Returning_sut of string
  | No_sut_argument of string
  | Multiple_sut_arguments of string
  | No_sut_type of string
  | No_init_function of string
  | Syntax_error_in_type of string
  | Syntax_error_in_init_sut of string
  | Sut_type_not_supported of string
  | Init_sut_not_supported of string
  | Type_parameter_not_instantiated of string
  | Type_not_supported_for_sut_parameter of string
  | Incompatible_type of (string * string)
  | Sut_type_not_specified of string
  | No_models of string
  | No_spec of string
  | Impossible_term_substitution of (string * [ `New | `Old | `NotModel ])
  | Ignored_modifies
  | Ensures_not_found_for_next_state of (string * string)
  | Type_not_supported of string
  | Impossible_init_state_generation of init_state_error
  | Functional_argument of string
  | Ghost_values of (string * [ `Arg | `Ret ])

type 'a reserr

val ok : 'a -> 'a reserr
val error : W.t -> 'a reserr
val warns : W.t list -> unit reserr
val warn : W.t -> unit reserr
val ( let* ) : 'a reserr -> ('a -> 'b reserr) -> 'b reserr
val ( >>= ) : 'a reserr -> ('a -> 'b reserr) -> 'b reserr
val ( and* ) : 'a reserr -> 'b reserr -> ('a * 'b) reserr

val sequence : 'a reserr list -> 'a list reserr
(** [sequence rs] returns [ok] of the list of ['a] iff there is no [error] in
    [rs] *)

val promote : 'a reserr list -> 'a list reserr
(** [promote rs] filters [rs] and returns [ok] of the list of ['a] iff there is
    no [errors] of level [Error] in [rs] and store the [errors] of level
    [Warning] in the warnings list *)

val promote_opt : 'a reserr -> 'a option reserr
(** [promote_opt r] is [promote] for a unique value *)

val of_option : default:W.t -> 'a option -> 'a reserr
val to_option : 'a reserr -> 'a option
val map : ('a -> 'b reserr) -> 'a list -> 'b list reserr
val concat_map : ('a -> 'b list reserr) -> 'a list -> 'b list reserr
val fmap : ('a -> 'b) -> 'a reserr -> 'b reserr
val ( <$> ) : ('a -> 'b) -> 'a reserr -> 'b reserr
val app : ('a -> 'b) reserr -> 'a reserr -> 'b reserr
val ( <*> ) : ('a -> 'b) reserr -> 'a reserr -> 'b reserr
val pp : 'a Fmt.t -> 'a reserr Fmt.t

module W = Ortac_core.Warnings

type init_state_error =
  | Mismatch_number_of_arguments of string
  | No_appropriate_specifications of string * string list
  | No_specification of string
  | No_translatable_specification of string
  | Not_a_function_call of string
  | Not_returning_sut of string
  | Qualified_name of string

type W.kind +=
  | Constant_value of string
  | Ensures_not_found_for_next_state of (string * string)
  | Ensures_not_found_for_ret_sut of (string * string list)
  | Functional_argument of string
  | Ghost_values of (string * [ `Arg | `Ret ])
  | Impossible_init_state_generation of init_state_error
  | Impossible_term_substitution of
      [ `Never | `New | `Old | `NotModel | `OutOfScope ]
  | Incompatible_sut of string
  | Incompatible_type of (string * string)
  | Incomplete_configuration_module of [ `Init_sut | `Sut ]
  | Incomplete_ret_val_computation of string
  | No_configuration_file of string
  | No_init_function of string
  | No_models of string
  | No_spec of string
  | No_sut_type of string
  | Not_a_structure of string
  | Returning_nested_sut of string
  | Sut_as_type_inst of string
  | Sut_in_tuple of string
  | Sut_type_not_specified of string
  | Sut_type_not_supported of string
  | Syntax_error_in_config_module of string
  | Tuple_arity of string
  | Type_not_supported of string
  | Type_not_supported_for_sut_parameter of string
  | Type_parameter_not_instantiated of string

type 'a reserr

val ok : 'a -> 'a reserr
val error : W.t -> 'a reserr
val warns : W.t list -> unit reserr
val warn : W.t -> unit reserr
val ( let* ) : 'a reserr -> ('a -> 'b reserr) -> 'b reserr
val ( >>= ) : 'a reserr -> ('a -> 'b reserr) -> 'b reserr
val ( and* ) : 'a reserr -> 'b reserr -> ('a * 'b) reserr

val traverse : ('a -> 'b reserr) -> 'a list -> 'b list reserr
(** [traverse f xs] maps [f] over [xs] and returns [ok] of the resulting list
    iff it contains no [error] *)

val traverse_ : ('a -> 'b reserr) -> 'a list -> unit reserr
(** [traverse_ f xs] is [traverse f xs] ignoring the returned list *)

val sequence : 'a reserr list -> 'a list reserr
(** [sequence rs] returns [ok] of the list of ['a] iff there is no [error] in
    [rs] *)

val promote : 'a reserr list -> 'a list reserr
(** [promote rs] filters [rs] and returns [ok] of the list of ['a] iff there is
    no [errors] of level [Error] in [rs] and store the [errors] of level
    [Warning] in the warnings list *)

val promote_map : ('a -> 'b reserr) -> 'a list -> 'b list reserr
(** [promote_map f xs] is [List.map f xs |> promote] with only one traversal *)

val promote_mapi : (int -> 'a -> 'b reserr) -> 'a list -> 'b list reserr
(** [promote_mapi f xs] is [List.mapi f xs |> promote] with only one traversal *)

val promote_opt : 'a reserr -> 'a option reserr
(** [promote_opt r] is [promote] for a unique value *)

val fold_left : ('a -> 'b -> 'a reserr) -> 'a -> 'b list -> 'a reserr
val of_option : default:W.t -> 'a option -> 'a reserr
val to_option : 'a reserr -> 'a option
val fmap : ('a -> 'b) -> 'a reserr -> 'b reserr
val ( <$> ) : ('a -> 'b) -> 'a reserr -> 'b reserr
val app : ('a -> 'b) reserr -> 'a reserr -> 'b reserr
val ( <*> ) : ('a -> 'b) reserr -> 'a reserr -> 'b reserr
val pp : bool -> 'a Fmt.t -> 'a reserr Fmt.t

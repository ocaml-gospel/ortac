module W = Ortac_core.Warnings

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
  | Incompatible_type of string
  | Sut_type_not_specified of string
  | No_models of string

type 'a reserr

val ok : 'a -> 'a reserr
val error : W.t -> 'a reserr
val warns : W.t list -> unit reserr
val warn : W.t -> unit reserr
val ( let* ) : 'a reserr -> ('a -> 'b reserr) -> 'b reserr
val ( and* ) : 'a reserr -> 'b reserr -> ('a * 'b) reserr
val promote : 'a reserr list -> 'a list reserr
val map : ('a -> 'b reserr) -> 'a list -> 'b list reserr
val fmap : ('a -> 'b) -> 'a reserr -> 'b reserr
val ( <$> ) : ('a -> 'b) -> 'a reserr -> 'b reserr
val pp : 'a Fmt.t -> 'a reserr Fmt.t

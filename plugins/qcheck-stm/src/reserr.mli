module W = Ortac_core.Warnings

type W.kind +=
  | Constant_value of string
  | Returning_sut of string
  | No_sut_argument of string
  | Multiple_sut_arguments of string

type 'a reserr

val ok : 'a -> 'a reserr
val error : W.t -> 'a reserr
val ( let* ) : 'a reserr -> ('a -> 'b reserr) -> 'b reserr
val ( and* ) : 'a reserr -> 'b reserr -> ('a * 'b) reserr
val pp : 'a Fmt.t -> 'a reserr Fmt.t

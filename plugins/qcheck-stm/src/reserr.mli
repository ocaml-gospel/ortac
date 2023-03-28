module Ident = Gospel.Identifier.Ident

type error =
  | Value_is_a_constant of (Ppxlib.location * Ident.t)
  | Value_return_sut of (Ppxlib.location * Ident.t)
  | Value_have_no_sut_argument of (Ppxlib.location * Ident.t)
  | Value_have_multiple_sut_arguments of (Ppxlib.location * Ident.t)

type 'a reserr

val ok : 'a -> 'a reserr
val error : error -> 'a reserr
val ( let* ) : 'a reserr -> ('a -> 'b reserr) -> 'b reserr
val ( and* ) : 'a reserr -> 'b reserr -> ('a * 'b) reserr
val to_string : ('a -> string) -> 'a reserr -> string

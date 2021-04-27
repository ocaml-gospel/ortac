open Ppxlib
open Gospel

module Make (B : Backend.S) : sig
  exception Unsupported of Location.t option * string

  val returned_pattern : Tast.lb_arg list -> pattern * expression

  val mk_setup : location -> string -> (expression -> expression) * string

  val mk_pre_checks : label -> Tterm.term list -> expression -> expression

  val mk_call :
    label ->
    pattern ->
    location ->
    label ->
    (Ttypes.xsymbol * (Tterm.pattern * Tterm.term) list) list ->
    (arg_label * expression) list ->
    expression ->
    expression

  val mk_post_checks : label -> Tterm.term list -> expression -> expression
end

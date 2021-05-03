open Ppxlib
open Gospel

exception Unsupported of Location.t option * string

val returned_pattern : Tast.lb_arg list -> pattern * expression

val mk_setup : location -> string -> (expression -> expression) * string

val mk_pre_checks :
  register_name:expression -> Tterm.term list -> expression -> expression

val mk_call :
  register_name:expression ->
  pattern ->
  location ->
  label ->
  (Ttypes.xsymbol * (Tterm.pattern * Tterm.term) list) list ->
  (arg_label * expression) list ->
  expression ->
  expression

val mk_post_checks :
  register_name:expression -> Tterm.term list -> expression -> expression

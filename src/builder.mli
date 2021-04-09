open Ppxlib
open Gospel

include Ast_builder.S

val noloc : 'a -> 'a loc

val elocation : location -> expression

val econst : constant -> expression

val epred : expression -> expression

val esucc : expression -> expression

val efun : (arg_label * pattern) list -> expression -> expression

exception Unsupported of Location.t option * string

val lident : label -> longident loc

val returned_pattern : Tast.lb_arg list -> pattern * expression

val mk_open : structure_item

val mk_setup : location -> (expression -> expression) * label * label

val mk_pre_checks :
  label -> label -> expression -> Tterm.term list -> expression -> expression

val mk_call :
  label ->
  pattern ->
  location ->
  label ->
  expression ->
  (Ttypes.xsymbol * (Tterm.pattern * Tterm.term) list) list ->
  (arg_label * expression) list ->
  expression ->
  expression

val mk_post_checks :
  label -> label -> expression -> Tterm.term list -> expression -> expression

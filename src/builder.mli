open Ppxlib
open Gospel

include Ast_builder.S

val noloc : 'a -> 'a loc

val elocation : location -> expression

val econst : constant -> expression

val epred : expression -> expression

val esucc : expression -> expression

val failed_pre :
  string -> string -> expression -> Gospel.Tterm.term -> expression

val failed_post :
  string -> string -> expression -> Gospel.Tterm.term -> expression

val failed_post_nonexec :
  string ->
  expression ->
  string ->
  expression ->
  Gospel.Tterm.term ->
  expression

val failed_pre_nonexec :
  string ->
  expression ->
  string ->
  expression ->
  Gospel.Tterm.term ->
  expression

val failed_xpost :
  string -> string -> expression -> Gospel.Tterm.term -> expression

val failed_xpost_nonexec :
  string ->
  expression ->
  string ->
  expression ->
  Gospel.Tterm.term ->
  expression

val efun : (arg_label * pattern) list -> expression -> expression

exception Unsupported of Location.t option * string

val lident : label -> longident loc

val post : label -> label -> expression -> Tterm.term list -> expression

val pre : label -> label -> expression -> Tterm.term list -> expression

val xpost_guard :
  label ->
  'a ->
  label ->
  expression ->
  (Ttypes.xsymbol * (Tterm.pattern * Tterm.term) list) list ->
  expression ->
  expression

val of_gospel_args :
  Tast.lb_arg list -> (arg_label * expression) list * (arg_label * pattern) list

val returned_pattern : Tast.lb_arg list -> pattern * expression

val mk_open : structure_item

open Ppxlib

include Ast_builder.S

val noloc : 'a -> 'a loc

val elocation : location -> expression

val econst : constant -> expression

val epred : expression -> expression

val esucc : expression -> expression

val failed_pre : string -> expression -> Gospel.Tterm.term -> expression

val failed_post : string -> expression -> Gospel.Tterm.term -> expression

val failed_post_nonexec :
  expression -> string -> expression -> Gospel.Tterm.term -> expression

val failed_pre_nonexec :
  expression -> string -> expression -> Gospel.Tterm.term -> expression

val failed_xpost : string -> expression -> Gospel.Tterm.term -> expression

val failed_xpost_nonexec :
  expression -> string -> expression -> Gospel.Tterm.term -> expression

val efun : (arg_label * pattern) list -> expression -> expression

open Ppxlib

include Ast_builder.S

val noloc : 'a -> 'a loc

val econst : Gospel.Oasttypes.constant -> expression

val eand : expression -> expression -> expression

val eor : expression -> expression -> expression

val enot : expression -> expression

val failed_pre : string -> Gospel.Tterm.term -> expression

val failed_post : string -> Gospel.Tterm.term -> expression

val failed_post_nonexec :
  string -> Gospel.Tterm.term -> expression -> expression

val failed_pre_nonexec : string -> Gospel.Tterm.term -> expression -> expression

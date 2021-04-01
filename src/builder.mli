open Ppxlib

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

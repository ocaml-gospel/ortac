open Ppxlib
open Gospel

type config

val config : Drv.t -> Tterm.vsymbol list -> config

val returned_pattern : Tast.lb_arg list -> pattern * expression

val mk_setup : location -> string -> (expression -> expression) * string

val mk_pre_checks :
  config:config ->
  register_name:expression ->
  term_printer:(Tterm.term -> string) ->
  Tterm.term list ->
  expression ->
  expression

val mk_call :
  config:config ->
  register_name:expression ->
  term_printer:(Tterm.term -> string) ->
  pattern ->
  location ->
  label ->
  (Ttypes.xsymbol * (Tterm.pattern * Tterm.term) list) list ->
  (arg_label * expression) list ->
  expression ->
  expression

val mk_post_checks :
  config:config ->
  register_name:expression ->
  term_printer:(Tterm.term -> string) ->
  Tterm.term list ->
  expression ->
  expression

val mk_function_def : config:config -> Tterm.term -> expression option

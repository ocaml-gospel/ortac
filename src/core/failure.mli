open Ppxlib
open Gospel

val violated :
  [ `Post | `Pre | `XPost ] ->
  term:Tterm.term ->
  register_name:string ->
  expression

val spec_failure :
  [ `Post | `Pre | `XPost ] ->
  term:Tterm.term ->
  exn:expression ->
  register_name:string ->
  expression

val unexpected_exn :
  allowed_exn:string list ->
  exn:expression ->
  register_name:string ->
  expression

val uncaught_checks : term:Tterm.term -> register_name:string -> expression

val unexpected_checks :
  terms:Tterm.term list -> register_name:string -> expression

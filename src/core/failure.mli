open Ppxlib

val violated :
  [ `Post | `Pre | `XPost ] ->
  term:string ->
  register_name:expression ->
  expression

val violated_invariant :
  state:expression ->
  typ:string ->
  term:string ->
  register_name:expression ->
  expression

val spec_failure :
  [ `Post | `Pre | `XPost | `Invariant ] ->
  term:string ->
  exn:expression ->
  register_name:expression ->
  expression

val unexpected_exn :
  allowed_exn:string list ->
  exn:expression ->
  register_name:expression ->
  expression

val uncaught_checks : term:string -> register_name:expression -> expression

val unexpected_checks :
  terms:string list -> register_name:expression -> expression

val report : register_name:expression -> expression

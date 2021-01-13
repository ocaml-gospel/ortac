open Ppxlib

include Ast_builder.S

val noloc : 'a -> 'a loc

val econst : constant -> expression

val eand : expression -> expression -> expression

val eor : expression -> expression -> expression

val enot : expression -> expression

val epred : expression -> expression

val esucc : expression -> expression

val failed_pre : string -> Gospel.Tterm.term -> expression

val failed_post : string -> Gospel.Tterm.term -> expression

val failed_post_nonexec :
  expression -> string -> Gospel.Tterm.term -> expression

val failed_pre_nonexec : expression -> string -> Gospel.Tterm.term -> expression

val check_exceptions : location -> string -> expression -> cases -> expression
(** Builds an AST fragment wrapping an expression in a [try...with].
    [check_exceptions loc fun_name call \[\]] is:

    {[
      try call with
      | (Stack_overflow | Out_of_memory) as e -> raise e
      | _ as e -> Gospel_runtime.unexpected_exn loc fun_name e
    ]}

    Each case provided is added at the beginning of the error matching. *)

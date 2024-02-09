open Ppxlib
include Ast_builder.S

val noloc : 'a -> 'a loc
val qualify : string list -> string -> expression
val epred : expression -> expression
val esucc : expression -> expression
val econst : constant -> expression
val enot : expression -> expression
val elocation : location -> expression
val efun : (arg_label * pattern) list -> expression -> expression
val lident : label -> longident loc
val list_fold_expr : expression -> string -> expression list -> expression
val list_and : expression list -> expression
val list_or : expression list -> expression
val enone : expression
val esome : expression -> expression

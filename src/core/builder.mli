open Ppxlib
include Ast_builder.S

val noloc : 'a -> 'a loc
val epred : expression -> expression
val esucc : expression -> expression
val econst : constant -> expression
val elocation : location -> expression
val efun : (arg_label * pattern) list -> expression -> expression
val lident : label -> longident loc

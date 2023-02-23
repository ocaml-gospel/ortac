module W = Warnings
open Gospel

val with_checks :
  context:Context.t ->
  term_printer:(Tterm.term -> string) ->
  Tterm.term list ->
  Translated.value ->
  Translated.value

val with_pres :
  context:Context.t ->
  term_printer:(Tterm.term -> string) ->
  Tterm.term list ->
  Translated.value ->
  Translated.value

val with_models :
  context:Context.t ->
  (Symbols.lsymbol * bool) list ->
  Translated.type_ ->
  Translated.type_

val with_invariants :
  context:Context.t ->
  term_printer:(Gospel.Tterm.term -> string) ->
  Gospel.Symbols.vsymbol option * Gospel.Tterm.term list ->
  Translated.type_ ->
  Translated.type_

val with_consumes :
  Gospel.Tterm.term list -> Translated.value -> Translated.value

val with_modified :
  Gospel.Tterm.term list -> Translated.value -> Translated.value

val with_posts :
  context:Context.t ->
  term_printer:(Tterm.term -> string) ->
  Tterm.term list ->
  Translated.value ->
  Translated.value

val with_constant_checks :
  context:Context.t ->
  term_printer:(Tterm.term -> string) ->
  Tterm.term list ->
  Translated.constant ->
  Translated.constant

val with_xposts :
  context:Context.t ->
  term_printer:(Tterm.term -> string) ->
  (Ttypes.xsymbol * (Tterm.pattern * Tterm.term) list) list ->
  Translated.value ->
  Translated.value

val function_definition :
  context:Context.t -> Symbols.lsymbol -> string -> Tterm.term -> Translated.term

val axiom_definition :
  context:Context.t -> register_name:string -> Tterm.term -> Translated.term

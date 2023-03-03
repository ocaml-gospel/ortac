val pattern : Gospel.Tterm.pattern_node -> Ppxlib.pattern
(** [pattern pn] translates a Gospel pattern into the matching OCaml pattern *)

val term : context:Context.t -> Gospel.Tterm.term -> Ppxlib.expression
(** [term ~context t] translated a Gospel typed term into the corresponding
    OCaml value using the translation [context] *)

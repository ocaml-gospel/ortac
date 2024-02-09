type plugins

val plugins : plugins
val register : unit Cmdliner.Cmd.t -> unit
val fold : ('a -> unit Cmdliner.Cmd.t -> 'a) -> 'a -> plugins -> 'a
val get_out_formatter : string option -> Format.formatter
val setup_log : unit Cmdliner.Term.t
val include_ : string option Cmdliner.Term.t
val output_file : string option Cmdliner.Term.t
val ocaml_file : string Cmdliner.Term.t
val quiet : bool Cmdliner.Term.t

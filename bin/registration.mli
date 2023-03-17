type plugins

val plugins : plugins
val register : unit Cmdliner.Cmd.t -> unit
val fold : ('a -> unit Cmdliner.Cmd.t -> 'a) -> 'a -> plugins -> 'a
val get_channel : string option -> out_channel
val setup_log : unit Cmdliner.Term.t
val output_file : string option Cmdliner.Term.t
val ocaml_file : string Cmdliner.Term.t

type plugins

val plugins : plugins
val register : unit Cmdliner.Cmd.t -> unit
val fold : ('a -> unit Cmdliner.Cmd.t -> 'a) -> 'a -> plugins -> 'a

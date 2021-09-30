val generate : string -> out_channel -> unit
(** [generate path output] generate the code of the tests corresponding to the
    specifications present in [path] in the monolith configuration and print it
    on the [output] channel *)

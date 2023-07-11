val signature :
  runtime:string ->
  module_name:string ->
  Gospel.Tmodule.namespace ->
  Gospel.Tast.signature_item list ->
  Ppxlib.structure

val generate : string -> out_channel -> unit
(** [generate path out] generate the code of the tests corresponding to the
    specifications present in [path] in the default configuration and print it
    on the [out] channel *)

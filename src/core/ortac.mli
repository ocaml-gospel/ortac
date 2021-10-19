module Make (G : Frontend.S) : sig
  val signature :
    string ->
    Gospel.Tmodule.namespace ->
    Gospel.Tast.signature_item list ->
    Ppxlib.structure

  val report :
    string ->
    Gospel.Tmodule.namespace ->
    Gospel.Tast.signature_item list ->
    unit
end

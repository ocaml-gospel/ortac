module Make (G : Frontend.S) : sig
  val signature :
    string ->
    Gospel.Tmodule.namespace list ->
    Gospel.Tast.signature ->
    Ppxlib.structure
  (** [signature s] generate the representation of the test file corresponding
      to [s] *)
end

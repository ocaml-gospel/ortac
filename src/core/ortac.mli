module Make (G : Backend.S) : sig
  val signature : string -> Gospel.Tast.signature -> Ppxlib.structure
  (** [signature s] generate the representation of the test file corresponding
      to [s] *)
end

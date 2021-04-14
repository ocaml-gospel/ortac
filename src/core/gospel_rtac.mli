module type G = Config_intf.S

module Make (G : G) : sig
  val signature : Gospel.Tast.signature_item list -> Ppxlib.structure_item list
  (** [signature s] generate the representation of the test file corresponding
      to [s] *)
end

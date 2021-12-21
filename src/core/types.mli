module Mutability : sig
  val ty : driver:Drv.t -> Gospel.Ttypes.ty -> Translated.mutability

  val type_declaration :
    driver:Drv.t -> Gospel.Tast.type_declaration -> Translated.mutability

  val type_spec : driver:Drv.t -> Gospel.Tast.type_spec -> Translated.mutability
end

module Equality : sig
  module W = Warnings
  open Ppxlib

  val derive : Translated.type_ -> (expression, W.t) result
  (** [derive t] derive the equality function that can be used in runtime
      assertion checking. *)
end

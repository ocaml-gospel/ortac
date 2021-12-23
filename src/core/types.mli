module Mutability : sig
  val ty : driver:Drv.t -> Gospel.Ttypes.ty -> Translated.mutability

  val type_declaration :
    driver:Drv.t -> Gospel.Tast.type_declaration -> Translated.mutability

  val type_spec : driver:Drv.t -> Gospel.Tast.type_spec -> Translated.mutability
end

open Ppxlib

module Equality : sig
  val derive : Translated.type_ -> (expression, Ortac_core__Warnings.t) result
  (** [derive t] derive the equality function that can be used in runtime
      assertion checking. *)
end

module Comparison : sig
  val derive : Translated.type_ -> (expression, Ortac_core__Warnings.t) result
  (** [derive t] derive the comparison function that can be used in runtime
      assertion checking. *)
end

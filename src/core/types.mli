module Mutability : sig
  val ty : driver:Drv.t -> Gospel.Ttypes.ty -> Translated.mutability

  val type_declaration :
    driver:Drv.t -> Gospel.Tast.type_declaration -> Translated.mutability

  val type_spec : driver:Drv.t -> Gospel.Tast.type_spec -> Translated.mutability
end

module Comparison : sig
  val derive : Translated.type_ -> (Ppxlib.expression, Warnings.kind) result
end

module Equality : sig
  val derive : Translated.type_ -> (Ppxlib.expression, Warnings.kind) result
end

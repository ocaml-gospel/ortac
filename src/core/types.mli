module Mutability : sig
  val ty : driver:Drv.t -> Gospel.Ttypes.ty -> Translated.mutability

  val type_declaration :
    driver:Drv.t -> Gospel.Tast.type_declaration -> Translated.mutability

  val mutable_model :
    driver:Drv.t -> (Gospel.Tterm.lsymbol * bool) list -> Translated.mutability
end

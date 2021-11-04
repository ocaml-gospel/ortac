module Mutability : sig
  val ty : driver:Drv.t -> Gospel.Ttypes.ty -> bool
  val type_declaration : driver:Drv.t -> Gospel.Tast.type_declaration -> bool
  val mutable_model : driver:Drv.t -> (Gospel.Tterm.lsymbol * bool) list -> bool
end

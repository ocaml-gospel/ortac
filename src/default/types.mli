module Mutability : sig
  val ty : ir:Ir.t -> Gospel.Ttypes.ty -> Ir.mutability

  val type_declaration :
    ir:Ir.t -> Gospel.Tast.type_declaration -> Ir.mutability

  val type_spec : ir:Ir.t -> Gospel.Tast.type_spec -> Ir.mutability
end

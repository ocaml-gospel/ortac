module Mutability : sig
  val ty : ir:Translated.t -> Gospel.Ttypes.ty -> Translated.mutability

  val type_declaration :
    ir:Translated.t -> Gospel.Tast.type_declaration -> Translated.mutability

  val type_spec : ir:Translated.t -> Gospel.Tast.type_spec -> Translated.mutability
end

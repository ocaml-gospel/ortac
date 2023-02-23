module Mutability : sig
  val ty : context:Context.t -> Gospel.Ttypes.ty -> Translated.mutability

  val type_declaration :
    context:Context.t -> Gospel.Tast.type_declaration -> Translated.mutability

  val type_spec : context:Context.t -> Gospel.Tast.type_spec -> Translated.mutability
end

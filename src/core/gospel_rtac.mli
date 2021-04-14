module type G = Config_intf.S

module Make (G : G) : sig
  val of_gospel_args :
    Gospel.Tast.lb_arg list ->
    (Ppxlib.arg_label * Ppxlib.expression) list
    * (Ppxlib.arg_label * Ppxlib.pattern) list

  val value : Gospel.Tast.val_description -> Ppxlib.structure_item option

  val signature : Gospel.Tast.signature_item list -> Ppxlib.structure_item list
end

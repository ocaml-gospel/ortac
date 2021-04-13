module type G = sig
  val open_modules : Ppxlib.structure_item list

  val report_pre : string -> Ppxlib.expression

  val report_post : string -> Ppxlib.expression

  val report_declared_exn : string -> string -> Ppxlib.expression

  val report_undeclared_exn :
    Ppxlib.expression -> string -> string -> Ppxlib.expression
end

module Make (G : G) : sig
  val of_gospel_args :
    Gospel.Tast.lb_arg list ->
    (Ppxlib.arg_label * Ppxlib.expression) list
    * (Ppxlib.arg_label * Ppxlib.pattern) list

  val value : Gospel.Tast.val_description -> Ppxlib.structure_item option

  val signature : Gospel.Tast.signature_item list -> Ppxlib.structure_item list
end

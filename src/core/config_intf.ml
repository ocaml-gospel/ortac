module type S = sig
  val open_modules : Ppxlib.structure_item

  val report_pre : string -> Ppxlib.expression

  val report_post : string -> Ppxlib.expression

  val report_declared_exn : string -> string -> Ppxlib.expression

  val report_undeclared_exn :
    Ppxlib.expression -> string -> string -> Ppxlib.expression
end

val is_derivable : Gospel.Tterm.lsymbol -> bool

val derive :
  Drv.t -> Gospel.Tterm.lsymbol -> (Ppxlib.expression, Warnings.kind) result

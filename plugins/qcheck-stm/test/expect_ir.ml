open Ortac_qcheck_stm

let () =
  let pp = Fmt.(list (Reserr.pp Ir.pp_value) stdout) in
  Ir_of_gospel.run "lib.mli" "good_init" "sut" |> pp

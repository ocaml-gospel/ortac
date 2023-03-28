open Ortac_qcheck_stm

let pp r = Reserr.to_string Ir.Pp.value r |> print_endline
let () = Ir_of_gospel.run "lib.mli" "good_init" "sut" |> List.iter pp

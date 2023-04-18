open Ortac_qcheck_stm

let () =
  let pp = Fmt.((Reserr.pp (list Ir.pp_value)) stdout) in
  Ir_of_gospel.run "lib.mli" "make 16 'a'" "(char, int) t" |> pp

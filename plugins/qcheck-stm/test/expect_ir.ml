open Ortac_qcheck_stm

let _ =
  let open Reserr in
  let pp = Fmt.((pp Ir.pp) stdout) in
  let* config, sigs = Config.init "lib.mli" "make 16 'a'" "(char, int) t" in
  Ir_of_gospel.run config sigs |> pp;
  ok ()

open Fmt
open Ortac_qcheck_stm

let _ =
  let open Reserr in
  let* sigs, config = Config.init "lib.mli" "make 'a' 42" "(char, int) t" in
  let* ir = Ir_of_gospel.signature config sigs in
  let stm = Stm_of_ir.cmd_type config ir in
  pf stdout "%a@." Ppxlib_ast.Pprintast.structure [ stm ];
  ok ()

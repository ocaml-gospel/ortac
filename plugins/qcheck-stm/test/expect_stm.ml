open Fmt
open Ortac_qcheck_stm

let _ =
  let open Reserr in
  let* sigs, config = Config.init "lib.mli" "make 'a' 42" "(char, int) t" in
  let* ir = Ir_of_gospel.run sigs config in
  let cmd = Stm_of_ir.cmd_type config ir in
  let state = Stm_of_ir.state_type ir in
  pf stdout "%a@." Ppxlib_ast.Pprintast.structure [ cmd; state ];
  ok ()

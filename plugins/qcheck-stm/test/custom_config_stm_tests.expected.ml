(* This file is generated by ortac qcheck-stm,
   edit how you run the tool instead *)
[@@@ocaml.warning "-26-27"]
open Custom_config
module Ortac_runtime = Ortac_runtime_qcheck_stm
module Spec =
  struct
    open STM
    module QCheck =
      struct
        include QCheck
        module Gen = struct include Gen
                            let int = small_signed_int end
      end
    type sut = int t
    type cmd =
      | Push of int 
    let show_cmd cmd__001_ =
      match cmd__001_ with
      | Push a_1 ->
          Format.asprintf "%s sut %a" "push" (Util.Pp.pp_int true) a_1
    type nonrec state = {
      contents: int Ortac_runtime.Gospelstdlib.sequence }
    let init_state =
      let () = () in
      {
        contents =
          (try Ortac_runtime.Gospelstdlib.Sequence.empty
           with
           | e ->
               raise
                 (Ortac_runtime.Partial_function
                    (e,
                      {
                        Ortac_runtime.start =
                          {
                            pos_fname = "custom_config.mli";
                            pos_lnum = 6;
                            pos_bol = 257;
                            pos_cnum = 282
                          };
                        Ortac_runtime.stop =
                          {
                            pos_fname = "custom_config.mli";
                            pos_lnum = 6;
                            pos_bol = 257;
                            pos_cnum = 296
                          }
                      })))
      }
    let init_sut () = empty ()
    let cleanup _ = ()
    let arb_cmd _ =
      let open QCheck in
        make ~print:show_cmd
          (let open Gen in oneof [(pure (fun a_1 -> Push a_1)) <*> int])
    let next_state cmd__002_ state__003_ =
      match cmd__002_ with
      | Push a_1 ->
          {
            contents =
              ((try
                  Ortac_runtime.Gospelstdlib.Sequence.cons a_1
                    state__003_.contents
                with
                | e ->
                    raise
                      (Ortac_runtime.Partial_function
                         (e,
                           {
                             Ortac_runtime.start =
                               {
                                 pos_fname = "custom_config.mli";
                                 pos_lnum = 11;
                                 pos_bol = 469;
                                 pos_cnum = 494
                               };
                             Ortac_runtime.stop =
                               {
                                 pos_fname = "custom_config.mli";
                                 pos_lnum = 11;
                                 pos_bol = 469;
                                 pos_cnum = 526
                               }
                           }))))
          }
    let precond cmd__008_ state__009_ =
      match cmd__008_ with | Push a_1 -> true
    let postcond _ _ _ = true
    let run cmd__010_ sut__011_ =
      match cmd__010_ with | Push a_1 -> Res (unit, (push sut__011_ a_1))
  end
module STMTests = (Ortac_runtime.Make)(Spec)
let check_init_state () = ()
let ortac_postcond cmd__004_ state__005_ res__006_ =
  let open Spec in
    let open STM in
      let new_state__007_ = lazy (next_state cmd__004_ state__005_) in
      match (cmd__004_, res__006_) with
      | (Push a_1, Res ((Unit, _), _)) -> None
      | _ -> None
let _ =
  QCheck_base_runner.run_tests_main
    (let count = 1000 in
     [STMTests.agree_test ~count ~name:"Custom_config STM tests"
        check_init_state ortac_postcond])
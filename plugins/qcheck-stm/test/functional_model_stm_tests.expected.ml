(* This file is generated by ortac qcheck-stm,
   edit how you run the tool instead *)
[@@@ocaml.warning "-26-27-69-32"]
open Functional_model
module Ortac_runtime = Ortac_runtime_qcheck_stm
module Spec =
  struct
    open STM
    type _ ty +=  
      | Integer: Ortac_runtime.integer ty 
    let integer = (Integer, Ortac_runtime.string_of_integer)
    type sut = (char, int) t
    type cmd =
      | Add of char * int 
    let show_cmd cmd__001_ =
      match cmd__001_ with
      | Add (a_1, b_1) ->
          Format.asprintf "%s sut %a %a" "add" (Util.Pp.pp_char true) a_1
            (Util.Pp.pp_int true) b_1
    type nonrec state = {
      m: char -> int option }
    let init_state =
      let () = () in
      {
        m =
          (try fun _ -> None
           with
           | e ->
               raise
                 (Ortac_runtime.Partial_function
                    (e,
                      {
                        Ortac_runtime.start =
                          {
                            pos_fname = "functional_model.mli";
                            pos_lnum = 6;
                            pos_bol = 275;
                            pos_cnum = 293
                          };
                        Ortac_runtime.stop =
                          {
                            pos_fname = "functional_model.mli";
                            pos_lnum = 6;
                            pos_bol = 275;
                            pos_cnum = 306
                          }
                      })))
      }
    let init_sut () = empty ()
    let cleanup _ = ()
    let arb_cmd _ =
      let open QCheck in
        make ~print:show_cmd
          (let open Gen in
             oneof
               [((pure (fun a_1 -> fun b_1 -> Add (a_1, b_1))) <*> char) <*>
                  int])
    let next_state cmd__002_ state__003_ =
      match cmd__002_ with
      | Add (a_1, b_1) ->
          {
            m =
              ((try fun x -> if x = a_1 then Some b_1 else state__003_.m x
                with
                | e ->
                    raise
                      (Ortac_runtime.Partial_function
                         (e,
                           {
                             Ortac_runtime.start =
                               {
                                 pos_fname = "functional_model.mli";
                                 pos_lnum = 11;
                                 pos_bol = 482;
                                 pos_cnum = 500
                               };
                             Ortac_runtime.stop =
                               {
                                 pos_fname = "functional_model.mli";
                                 pos_lnum = 11;
                                 pos_bol = 482;
                                 pos_cnum = 546
                               }
                           }))))
          }
    let precond cmd__008_ state__009_ =
      match cmd__008_ with | Add (a_1, b_1) -> true
    let postcond _ _ _ = true
    let run cmd__010_ sut__011_ =
      match cmd__010_ with
      | Add (a_1, b_1) -> Res (unit, (add sut__011_ a_1 b_1))
  end
module STMTests = (Ortac_runtime.Make)(Spec)
let check_init_state () = ()
let ortac_postcond cmd__004_ state__005_ res__006_ =
  let open Spec in
    let open STM in
      let new_state__007_ = lazy (next_state cmd__004_ state__005_) in
      match (cmd__004_, res__006_) with
      | (Add (a_1, b_1), Res ((Unit, _), _)) -> None
      | _ -> None
let _ =
  QCheck_base_runner.run_tests_main
    (let count = 1000 in
     [STMTests.agree_test ~count ~name:"Functional_model STM tests"
        check_init_state ortac_postcond])
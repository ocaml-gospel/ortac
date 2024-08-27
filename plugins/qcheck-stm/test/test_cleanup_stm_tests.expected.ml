(* This file is generated by ortac qcheck-stm,
   edit how you run the tool instead *)
[@@@ocaml.warning "-26-27-69-32"]
open Test_cleanup
module Ortac_runtime = Ortac_runtime_qcheck_stm
module SUT =
  (Ortac_runtime.SUT.Make)(struct type sut = t
                                  let init () = create () end)
module ModelElt =
  struct
    type nonrec elt = {
      m: Ortac_runtime.integer }
    let init =
      let () = () in
      {
        m =
          (try Ortac_runtime.Gospelstdlib.integer_of_int 0
           with
           | e ->
               raise
                 (Ortac_runtime.Partial_function
                    (e,
                      {
                        Ortac_runtime.start =
                          {
                            pos_fname = "test_cleanup.mli";
                            pos_lnum = 6;
                            pos_bol = 227;
                            pos_cnum = 245
                          };
                        Ortac_runtime.stop =
                          {
                            pos_fname = "test_cleanup.mli";
                            pos_lnum = 6;
                            pos_bol = 227;
                            pos_cnum = 246
                          }
                      })))
      }
  end
module Model = (Ortac_runtime.Model.Make)(ModelElt)
module Spec =
  struct
    open STM
    type _ ty +=  
      | Integer: Ortac_runtime.integer ty 
    let integer = (Integer, Ortac_runtime.string_of_integer)
    type sut = SUT.t
    let init_sut = SUT.create 1
    type state = Model.t
    let init_state = Model.create 1 ()
    type cmd =
      | Use 
    let show_cmd cmd__001_ =
      match cmd__001_ with | Use -> Format.asprintf "%s <sut>" "use"
    let cleanup t = ignore t
    let arb_cmd _ =
      let open QCheck in
        make ~print:show_cmd (let open Gen in oneof [pure Use])
    let next_state cmd__002_ state__003_ =
      match cmd__002_ with
      | Use ->
          let t_1__004_ = Model.get state__003_ 0 in
          let t_1__005_ =
            let open ModelElt in
              {
                m =
                  (try
                     Ortac_runtime.Gospelstdlib.(+)
                       (Ortac_runtime.Gospelstdlib.integer_of_int 1)
                       t_1__004_.m
                   with
                   | e ->
                       raise
                         (Ortac_runtime.Partial_function
                            (e,
                              {
                                Ortac_runtime.start =
                                  {
                                    pos_fname = "test_cleanup.mli";
                                    pos_lnum = 11;
                                    pos_bol = 377;
                                    pos_cnum = 397
                                  };
                                Ortac_runtime.stop =
                                  {
                                    pos_fname = "test_cleanup.mli";
                                    pos_lnum = 11;
                                    pos_bol = 377;
                                    pos_cnum = 398
                                  }
                              })))
              } in
          Model.push (Model.drop_n state__003_ 1) t_1__005_
    let precond cmd__011_ state__012_ =
      match cmd__011_ with
      | Use -> let t_1__013_ = Model.get state__012_ 0 in true
    let postcond _ _ _ = true
    let run cmd__014_ sut__015_ =
      match cmd__014_ with
      | Use ->
          Res
            (unit,
              (let t_1__016_ = SUT.pop sut__015_ in
               let res__017_ = use t_1__016_ in
               (SUT.push sut__015_ t_1__016_; res__017_)))
  end
module STMTests = (Ortac_runtime.Make)(Spec)
let check_init_state () = ()
let ortac_postcond cmd__006_ state__007_ res__008_ =
  let open Spec in
    let open STM in
      let new_state__009_ = lazy (next_state cmd__006_ state__007_) in
      match (cmd__006_, res__008_) with
      | (Use, Res ((Unit, _), _)) -> None
      | _ -> None
let _ =
  QCheck_base_runner.run_tests_main
    (let count = 1000 in
     [STMTests.agree_test ~count ~name:"Test_cleanup STM tests"
        check_init_state ortac_postcond])

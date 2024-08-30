(* This file is generated by ortac qcheck-stm,
   edit how you run the tool instead *)
[@@@ocaml.warning "-26-27-69-32"]
open Ref
module Ortac_runtime = Ortac_runtime_qcheck_stm
module SUT =
  (Ortac_runtime.SUT.Make)(struct type sut = t
                                  let init () = make 42 end)
module ModelElt =
  struct
    type nonrec elt = {
      value: Ortac_runtime.integer }
    let init =
      let i_1 = 42 in
      {
        value =
          (try Ortac_runtime.Gospelstdlib.integer_of_int i_1
           with
           | e ->
               raise
                 (Ortac_runtime.Partial_function
                    (e,
                      {
                        Ortac_runtime.start =
                          {
                            pos_fname = "ref.mli";
                            pos_lnum = 6;
                            pos_bol = 211;
                            pos_cnum = 233
                          };
                        Ortac_runtime.stop =
                          {
                            pos_fname = "ref.mli";
                            pos_lnum = 6;
                            pos_bol = 211;
                            pos_cnum = 234
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
      | Get 
    let show_cmd cmd__001_ =
      match cmd__001_ with | Get -> Format.asprintf "%s <sut>" "get"
    let cleanup _ = ()
    let arb_cmd _ =
      let open QCheck in
        make ~print:show_cmd (let open Gen in oneof [pure Get])
    let next_state cmd__002_ state__003_ =
      match cmd__002_ with
      | Get ->
          let r__004_ = Model.get state__003_ 0 in
          let r__005_ = r__004_ in
          Model.push (Model.drop_n state__003_ 1) r__005_
    let precond cmd__017_ state__018_ =
      match cmd__017_ with
      | Get -> let r__019_ = Model.get state__018_ 0 in true
    let postcond _ _ _ = true
    let run cmd__020_ sut__021_ =
      match cmd__020_ with
      | Get ->
          Res
            (int,
              (let r__022_ = SUT.pop sut__021_ in
               let res__023_ = get r__022_ in
               (SUT.push sut__021_ r__022_; res__023_)))
  end
module STMTests = (Ortac_runtime.Make)(Spec)
let check_init_state () = ()
let ortac_show_cmd cmd__025_ state__026_ =
  let open Spec in
    match cmd__025_ with
    | Get -> Format.asprintf "%s %s" "get" (SUT.get_name state__026_ 0)
let ortac_postcond cmd__006_ state__007_ res__008_ =
  let open Spec in
    let open STM in
      let new_state__009_ = lazy (next_state cmd__006_ state__007_) in
      match (cmd__006_, res__008_) with
      | (Get, Res ((Int, _), i)) ->
          if
            let r_old__012_ = Model.get state__007_ 0
            and r_new__013_ = lazy (Model.get (Lazy.force new_state__009_) 0) in
            (try
               (Ortac_runtime.Gospelstdlib.integer_of_int i) =
                 (Lazy.force r_new__013_).value
             with
             | e ->
                 raise
                   (Ortac_runtime.Partial_function
                      (e,
                        {
                          Ortac_runtime.start =
                            {
                              pos_fname = "ref.mli";
                              pos_lnum = 11;
                              pos_bol = 346;
                              pos_cnum = 358
                            };
                          Ortac_runtime.stop =
                            {
                              pos_fname = "ref.mli";
                              pos_lnum = 11;
                              pos_bol = 346;
                              pos_cnum = 369
                            }
                        })))
          then None
          else
            Some
              (Ortac_runtime.report "Ref" "make 42"
                 (Either.right
                    (Res
                       (integer,
                         (let r_old__010_ = Model.get state__007_ 0
                          and r_new__011_ =
                            lazy (Model.get (Lazy.force new_state__009_) 0) in
                          try (Lazy.force r_new__011_).value
                          with
                          | e ->
                              raise
                                (Ortac_runtime.Partial_function
                                   (e,
                                     {
                                       Ortac_runtime.start =
                                         {
                                           pos_fname = "ref.mli";
                                           pos_lnum = 11;
                                           pos_bol = 346;
                                           pos_cnum = 362
                                         };
                                       Ortac_runtime.stop =
                                         {
                                           pos_fname = "ref.mli";
                                           pos_lnum = 11;
                                           pos_bol = 346;
                                           pos_cnum = 369
                                         }
                                     })))))) "get"
                 [("i = r.value",
                    {
                      Ortac_runtime.start =
                        {
                          pos_fname = "ref.mli";
                          pos_lnum = 11;
                          pos_bol = 346;
                          pos_cnum = 358
                        };
                      Ortac_runtime.stop =
                        {
                          pos_fname = "ref.mli";
                          pos_lnum = 11;
                          pos_bol = 346;
                          pos_cnum = 369
                        }
                    })])
      | _ -> None
let _ =
  QCheck_base_runner.run_tests_main
    (let count = 1000 in
     [STMTests.agree_test ~count ~name:"Ref STM tests" 1 check_init_state
        ortac_show_cmd ortac_postcond])

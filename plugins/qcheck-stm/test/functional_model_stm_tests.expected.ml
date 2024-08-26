(* This file is generated by ortac qcheck-stm,
   edit how you run the tool instead *)
[@@@ocaml.warning "-26-27-69-32-38"]
open Functional_model
module Ortac_runtime = Ortac_runtime_qcheck_stm
module SUT =
  (Ortac_runtime.SUT.Make)(struct
                             type sut = (char, int) t
                             let init () = empty ()
                           end)
module ModelElt =
  struct
    type nonrec elt = {
      m: char -> int option }
    let init =
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
  end
module Model = (Ortac_runtime.Model.Make)(ModelElt)
module Spec =
  struct
    open STM
    type _ ty +=  
      | Integer: Ortac_runtime.integer ty 
    let integer = (Integer, Ortac_runtime.string_of_integer)
    type _ ty +=  
      | SUT: SUT.elt ty 
    let sut = (SUT, (fun _ -> "<sut>"))
    type sut = SUT.t
    let init_sut = SUT.create 1
    type state = Model.t
    let init_state = Model.create 1 ()
    type cmd =
      | Empty of unit 
      | Add of char * int 
    let show_cmd cmd__001_ =
      match cmd__001_ with
      | Empty () -> Format.asprintf "%s %a" "empty" (Util.Pp.pp_unit true) ()
      | Add (a_1, b_1) ->
          Format.asprintf "%s <sut> %a %a" "add" (Util.Pp.pp_char true) a_1
            (Util.Pp.pp_int true) b_1
    let cleanup _ = ()
    let arb_cmd _ =
      let open QCheck in
        make ~print:show_cmd
          (let open Gen in
             oneof
               [(pure (fun () -> Empty ())) <*> unit;
               ((pure (fun a_1 -> fun b_1 -> Add (a_1, b_1))) <*> char) <*>
                 int])
    let next_state cmd__002_ state__003_ =
      match cmd__002_ with
      | Empty () ->
          let t_1__005_ =
            let open ModelElt in
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
              } in
          Model.push (Model.drop_n state__003_ 0) t_1__005_
      | Add (a_1, b_1) ->
          let t_2__006_ = Model.get state__003_ 0 in
          let t_2__007_ =
            let open ModelElt in
              {
                m =
                  (try fun x -> if x = a_1 then Some b_1 else t_2__006_.m x
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
                              })))
              } in
          Model.push (Model.drop_n state__003_ 1) t_2__007_
    let precond cmd__013_ state__014_ =
      match cmd__013_ with
      | Empty () -> true
      | Add (a_1, b_1) -> let t_2__015_ = Model.get state__014_ 0 in true
    let postcond _ _ _ = true
    let run cmd__016_ sut__017_ =
      match cmd__016_ with
      | Empty () ->
          Res
            (sut,
              (let res__018_ = empty () in
               (SUT.push sut__017_ res__018_; res__018_)))
      | Add (a_1, b_1) ->
          Res
            (unit,
              (let t_2__019_ = SUT.pop sut__017_ in
               let res__020_ = add t_2__019_ a_1 b_1 in
               (SUT.push sut__017_ t_2__019_; res__020_)))
  end
module STMTests = (Ortac_runtime.Make)(Spec)
let check_init_state () = ()
let ortac_show_cmd cmd__022_ state__023_ =
  let open Spec in
    match cmd__022_ with
    | Empty () -> Format.asprintf "%s %a" "empty" (Util.Pp.pp_unit true) ()
    | Add (a_1, b_1) ->
        Format.asprintf "%s %s %a %a" "add" (SUT.get_name state__023_ 0)
          (Util.Pp.pp_char true) a_1 (Util.Pp.pp_int true) b_1
let ortac_postcond cmd__008_ state__009_ res__010_ =
  let open Spec in
    let open STM in
      let new_state__011_ = lazy (next_state cmd__008_ state__009_) in
      match (cmd__008_, res__010_) with
      | (Empty (), Res ((SUT, _), t_1)) -> None
      | (Add (a_1, b_1), Res ((Unit, _), _)) -> None
      | _ -> None
let _ =
  QCheck_base_runner.run_tests_main
    (let count = 1000 in
     [STMTests.agree_test ~count ~name:"Functional_model STM tests" 1
        check_init_state ortac_show_cmd ortac_postcond])

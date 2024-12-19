(* This file is generated by ortac qcheck-stm,
   edit how you run the tool instead *)
[@@@ocaml.warning "-26-27-69-32-38"]
open Submodule_and_prefix_lib.Submodule_and_prefix.M
module Ortac_runtime = Ortac_runtime_qcheck_stm
module SUT =
  (Ortac_runtime.SUT.Make)(struct type sut = int t
                                  let init () = make 0 end)
module ModelElt =
  struct
    type nonrec elt = {
      value: int }
    let init =
      let a_1 = 0 in
      {
        value =
          (try a_1
           with
           | e ->
               raise
                 (Ortac_runtime.Partial_function
                    (e,
                      {
                        Ortac_runtime.start =
                          {
                            pos_fname = "submodule_and_prefix.mli";
                            pos_lnum = 7;
                            pos_bol = 285;
                            pos_cnum = 309
                          };
                        Ortac_runtime.stop =
                          {
                            pos_fname = "submodule_and_prefix.mli";
                            pos_lnum = 7;
                            pos_bol = 285;
                            pos_cnum = 310
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
    let init_sut = SUT.create 0
    type state = Model.t
    let init_state = Model.create 0 ()
    type cmd =
      | Make of int 
    let show_cmd cmd__001_ =
      match cmd__001_ with
      | Make a_1 -> Format.asprintf "%s %a" "make" (Util.Pp.pp_int true) a_1
    let cleanup _ = ()
    let arb_cmd _ =
      let open QCheck in
        make ~print:show_cmd
          (let open Gen in
             oneof [(pure (fun a_1 -> Make a_1)) <*> small_signed_int])
    let next_state cmd__002_ state__003_ =
      match cmd__002_ with
      | Make a_1 ->
          let t_1__005_ =
            let open ModelElt in
              {
                value =
                  (try a_1
                   with
                   | e ->
                       raise
                         (Ortac_runtime.Partial_function
                            (e,
                              {
                                Ortac_runtime.start =
                                  {
                                    pos_fname = "submodule_and_prefix.mli";
                                    pos_lnum = 7;
                                    pos_bol = 285;
                                    pos_cnum = 309
                                  };
                                Ortac_runtime.stop =
                                  {
                                    pos_fname = "submodule_and_prefix.mli";
                                    pos_lnum = 7;
                                    pos_bol = 285;
                                    pos_cnum = 310
                                  }
                              })))
              } in
          Model.push (Model.drop_n state__003_ 0) t_1__005_
    let precond cmd__010_ state__011_ =
      match cmd__010_ with | Make a_1 -> true
    let postcond _ _ _ = true
    let run cmd__012_ sut__013_ =
      match cmd__012_ with
      | Make a_1 ->
          Res
            (sut,
              (let res__014_ = make a_1 in
               (SUT.push sut__013_ res__014_; res__014_)))
  end
module STMTests = (Ortac_runtime.Make)(Spec)
let check_init_state () = ()
let ortac_show_cmd cmd__016_ state__017_ last__019_ res__018_ =
  let open Spec in
    let open STM in
      match (cmd__016_, res__018_) with
      | (Make a_1, Res ((SUT, _), t_1)) ->
          let lhs = if last__019_ then "r" else SUT.get_name state__017_ 0
          and shift = 1 in
          Format.asprintf "let %s = %s %a" lhs "make" (Util.Pp.pp_int true)
            a_1
      | _ -> assert false
let ortac_postcond cmd__006_ state__007_ res__008_ =
  let open Spec in
    let open STM in
      let new_state__009_ = lazy (next_state cmd__006_ state__007_) in
      match (cmd__006_, res__008_) with
      | (Make a_1, Res ((SUT, _), t_1)) -> None
      | _ -> None
let _ =
  QCheck_base_runner.run_tests_main
    (let count = 1000 in
     [STMTests.agree_test ~count ~name:"Submodule_and_prefix STM tests" 0
        check_init_state ortac_show_cmd ortac_postcond])
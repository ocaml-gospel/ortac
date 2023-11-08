open Sequence_model
let length_opt s =
  try Some (Ortac_runtime.Gospelstdlib.Sequence.length s)
  with
  | e ->
      raise
        (Ortac_runtime.Partial_function
           (e,
             {
               Ortac_runtime.start =
                 {
                   pos_fname = "sequence_model.mli";
                   pos_lnum = 15;
                   pos_bol = 750;
                   pos_cnum = 756
                 };
               Ortac_runtime.stop =
                 {
                   pos_fname = "sequence_model.mli";
                   pos_lnum = 15;
                   pos_bol = 750;
                   pos_cnum = 760
                 }
             }))
module Spec =
  struct
    open STM
    [@@@ocaml.warning "-26-27"]
    type sut = char t
    type cmd =
      | Add of char 
      | Remove 
      | Remove_ 
    let show_cmd cmd__001_ =
      match cmd__001_ with
      | Add v -> Format.asprintf "%s %a" "add" (Util.Pp.pp_char true) v
      | Remove -> Format.asprintf "%s" "remove"
      | Remove_ -> Format.asprintf "%s" "remove_"
    type nonrec state = {
      contents: char Ortac_runtime.Gospelstdlib.sequence }
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
                            pos_fname = "sequence_model.mli";
                            pos_lnum = 6;
                            pos_bol = 263;
                            pos_cnum = 288
                          };
                        Ortac_runtime.stop =
                          {
                            pos_fname = "sequence_model.mli";
                            pos_lnum = 6;
                            pos_bol = 263;
                            pos_cnum = 302
                          }
                      })))
      }
    let init_sut () = create ()
    let cleanup _ = ()
    let arb_cmd _ =
      let open QCheck in
        make ~print:show_cmd
          (let open Gen in
             oneof
               [(pure (fun v -> Add v)) <*> char; pure Remove; pure Remove_])
    let next_state cmd__002_ state__003_ =
      match cmd__002_ with
      | Add v ->
          {
            contents =
              ((try
                  Ortac_runtime.Gospelstdlib.Sequence.cons v
                    state__003_.contents
                with
                | e ->
                    raise
                      (Ortac_runtime.Partial_function
                         (e,
                           {
                             Ortac_runtime.start =
                               {
                                 pos_fname = "sequence_model.mli";
                                 pos_lnum = 11;
                                 pos_bol = 475;
                                 pos_cnum = 500
                               };
                             Ortac_runtime.stop =
                               {
                                 pos_fname = "sequence_model.mli";
                                 pos_lnum = 11;
                                 pos_bol = 475;
                                 pos_cnum = 532
                               }
                           }))))
          }
      | Remove ->
          {
            contents =
              ((try
                  match Ortac_runtime.Gospelstdlib.Sequence.length
                          state__003_.contents
                  with
                  | __x__004_ when
                      (=) __x__004_
                        (Ortac_runtime.Gospelstdlib.integer_of_int 0)
                      -> Ortac_runtime.Gospelstdlib.Sequence.empty
                  | _ ->
                      Ortac_runtime.Gospelstdlib.Sequence.tl
                        state__003_.contents
                with
                | e ->
                    raise
                      (Ortac_runtime.Partial_function
                         (e,
                           {
                             Ortac_runtime.start =
                               {
                                 pos_fname = "sequence_model.mli";
                                 pos_lnum = 20;
                                 pos_bol = 953;
                                 pos_cnum = 978
                               };
                             Ortac_runtime.stop =
                               {
                                 pos_fname = "sequence_model.mli";
                                 pos_lnum = 22;
                                 pos_bol = 1070;
                                 pos_cnum = 1131
                               }
                           }))))
          }
      | Remove_ ->
          {
            contents =
              ((try
                  match length_opt state__003_.contents with
                  | Some __x__005_ when
                      (=) __x__005_
                        (Ortac_runtime.Gospelstdlib.integer_of_int 0)
                      -> Ortac_runtime.Gospelstdlib.Sequence.empty
                  | _ ->
                      Ortac_runtime.Gospelstdlib.Sequence.tl
                        state__003_.contents
                with
                | e ->
                    raise
                      (Ortac_runtime.Partial_function
                         (e,
                           {
                             Ortac_runtime.start =
                               {
                                 pos_fname = "sequence_model.mli";
                                 pos_lnum = 27;
                                 pos_bol = 1337;
                                 pos_cnum = 1362
                               };
                             Ortac_runtime.stop =
                               {
                                 pos_fname = "sequence_model.mli";
                                 pos_lnum = 29;
                                 pos_bol = 1454;
                                 pos_cnum = 1515
                               }
                           }))))
          }
    let precond cmd__010_ state__011_ =
      match cmd__010_ with | Add v -> true | Remove -> true | Remove_ -> true
    let postcond cmd__006_ state__007_ res__008_ =
      let new_state__009_ = lazy (next_state cmd__006_ state__007_) in
      match (cmd__006_, res__008_) with
      | (Add v, Res ((Unit, _), _)) -> true
      | (Remove, Res ((Option (Char), _), o)) -> true
      | (Remove_, Res ((Option (Char), _), o_1)) -> true
      | _ -> true
    let run cmd__012_ sut__013_ =
      match cmd__012_ with
      | Add v -> Res (unit, (add v sut__013_))
      | Remove -> Res ((option char), (remove sut__013_))
      | Remove_ -> Res ((option char), (remove_ sut__013_))
  end
module STMTests = (STM_sequential.Make)(Spec)
let _ =
  QCheck_base_runner.run_tests_main
    (let count = 1000 in
     [STMTests.agree_test ~count ~name:"Sequence_model STM tests"])

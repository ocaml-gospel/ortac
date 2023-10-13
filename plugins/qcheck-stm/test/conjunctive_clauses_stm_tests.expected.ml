open Conjunctive_clauses
let set_contents c i a_1 =
  Ortac_runtime.Gospelstdlib.List.mapi
    (fun j -> fun x -> if i = j then a_1 else x) c
module Spec =
  struct
    open STM
    [@@@ocaml.warning "-26-27"]
    type sut = char t
    type cmd =
      | Set of int * char 
    let show_cmd cmd__001_ =
      match cmd__001_ with
      | Set (i_1, a_2) ->
          Format.asprintf "%s %a %a" "set" (Util.Pp.pp_int true) i_1
            (Util.Pp.pp_char true) a_2
    type nonrec state = {
      contents: char list }
    let init_state =
      let i_2 = 42
      and a_3 = 'a' in
      {
        contents =
          (Ortac_runtime.Gospelstdlib.List.init
             (Ortac_runtime.Gospelstdlib.integer_of_int i_2) (fun _ -> a_3))
      }
    let init_sut () = make 42 'a'
    let cleanup _ = ()
    let arb_cmd _ =
      let open QCheck in
        make ~print:show_cmd
          (let open Gen in
             oneof
               [((pure (fun i_1 -> fun a_2 -> Set (i_1, a_2))) <*> int) <*>
                  char])
    let next_state cmd__002_ state__003_ =
      match cmd__002_ with
      | Set (i_1, a_2) ->
          {
            contents =
              (set_contents state__003_.contents
                 (Ortac_runtime.Gospelstdlib.integer_of_int i_1) a_2)
          }
    let precond cmd__008_ state__009_ =
      match cmd__008_ with | Set (i_1, a_2) -> true
    let postcond cmd__004_ state__005_ res__006_ =
      let new_state__007_ = lazy (next_state cmd__004_ state__005_) in
      match (cmd__004_, res__006_) with
      | (Set (i_1, a_2), Res ((Unit, _), _)) -> true
      | _ -> true
    let run cmd__010_ sut__011_ =
      match cmd__010_ with
      | Set (i_1, a_2) -> Res (unit, (set sut__011_ i_1 a_2))
  end
module STMTests = (STM_sequential.Make)(Spec)
let _ =
  QCheck_base_runner.run_tests_main
    (let count = 1000 in
     [STMTests.agree_test ~count ~name:"STM Lib test sequential"])

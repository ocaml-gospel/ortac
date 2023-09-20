open Hashtbl
let rec remove_first x xs_1 =
  match xs_1 with
  | (a_1, b_1)::xs ->
      if a_1 = x then xs else (a_1, b_1) :: (remove_first x xs)
  | [] -> []
module Spec =
  struct
    open STM
    [@@@ocaml.warning "-26-27"]
    type sut = (char, int) t
    type cmd =
      | Clear 
      | Reset 
      | Add of char * int 
      | Find of char 
      | Find_opt of char 
      | Find_all of char 
      | Mem of char 
      | Remove of char 
      | Replace of char * int 
      | Length 
    let show_cmd cmd__001_ =
      match cmd__001_ with
      | Clear -> Format.asprintf "%s" "clear"
      | Reset -> Format.asprintf "%s" "reset"
      | Add (a_2, b_2) ->
          Format.asprintf "%s %a %a" "add" (Util.Pp.pp_char true) a_2
            (Util.Pp.pp_int true) b_2
      | Find a_3 -> Format.asprintf "%s %a" "find" (Util.Pp.pp_char true) a_3
      | Find_opt a_4 ->
          Format.asprintf "%s %a" "find_opt" (Util.Pp.pp_char true) a_4
      | Find_all a_5 ->
          Format.asprintf "%s %a" "find_all" (Util.Pp.pp_char true) a_5
      | Mem a_6 -> Format.asprintf "%s %a" "mem" (Util.Pp.pp_char true) a_6
      | Remove a_7 ->
          Format.asprintf "%s %a" "remove" (Util.Pp.pp_char true) a_7
      | Replace (a_8, b_3) ->
          Format.asprintf "%s %a %a" "replace" (Util.Pp.pp_char true) a_8
            (Util.Pp.pp_int true) b_3
      | Length -> Format.asprintf "%s" "length"
    type nonrec state = {
      contents: (char * int) list }
    let init_state = let random = false
                     and size = 16 in { contents = [] }
    let init_sut () = create ~random:false 16
    let cleanup _ = ()
    let arb_cmd _ =
      let open QCheck in
        make ~print:show_cmd
          (let open Gen in
             oneof
               [pure Clear;
               pure Reset;
               ((pure (fun a_2 -> fun b_2 -> Add (a_2, b_2))) <*> char) <*>
                 int;
               (pure (fun a_3 -> Find a_3)) <*> char;
               (pure (fun a_4 -> Find_opt a_4)) <*> char;
               (pure (fun a_5 -> Find_all a_5)) <*> char;
               (pure (fun a_6 -> Mem a_6)) <*> char;
               (pure (fun a_7 -> Remove a_7)) <*> char;
               ((pure (fun a_8 -> fun b_3 -> Replace (a_8, b_3))) <*> char)
                 <*> int;
               pure Length])
    let next_state cmd__002_ state__003_ =
      match cmd__002_ with
      | Clear -> { contents = [] }
      | Reset -> { contents = [] }
      | Add (a_2, b_2) -> { contents = ((a_2, b_2) :: state__003_.contents) }
      | Find a_3 -> state__003_
      | Find_opt a_4 -> state__003_
      | Find_all a_5 -> state__003_
      | Mem a_6 -> state__003_
      | Remove a_7 -> { contents = (remove_first a_7 state__003_.contents) }
      | Replace (a_8, b_3) ->
          {
            contents =
              ((a_8, b_3) :: (remove_first a_8 state__003_.contents))
          }
      | Length -> state__003_
    let precond cmd__008_ state__009_ =
      match cmd__008_ with
      | Clear -> true
      | Reset -> true
      | Add (a_2, b_2) -> true
      | Find a_3 -> true
      | Find_opt a_4 -> true
      | Find_all a_5 -> true
      | Mem a_6 -> true
      | Remove a_7 -> true
      | Replace (a_8, b_3) -> true
      | Length -> true
    let postcond cmd__004_ state__005_ res__006_ =
      let new_state__007_ = lazy (next_state cmd__004_ state__005_) in
      match (cmd__004_, res__006_) with
      | (Clear, Res ((Unit, _), _)) -> true
      | (Reset, Res ((Unit, _), _)) -> true
      | (Add (a_2, b_2), Res ((Unit, _), _)) -> true
      | (Find a_3, Res ((Result (Int, Exn), _), b_4)) ->
          (match b_4 with
           | Ok b_4 ->
               Ortac_runtime.Gospelstdlib.List.mem (a_3, b_4)
                 (Lazy.force new_state__007_).contents
           | Error (Not_found) ->
               not
                 (Ortac_runtime.Gospelstdlib.List.mem a_3
                    (Ortac_runtime.Gospelstdlib.List.map
                       (fun x_1 -> Ortac_runtime.Gospelstdlib.fst x_1)
                       (Lazy.force new_state__007_).contents))
           | _ -> false)
      | (Find_opt a_4, Res ((Option (Int), _), o)) ->
          (match o with
           | None ->
               if
                 not
                   (Ortac_runtime.Gospelstdlib.List.mem a_4
                      (Ortac_runtime.Gospelstdlib.List.map
                         (fun x_2 -> Ortac_runtime.Gospelstdlib.fst x_2)
                         (Lazy.force new_state__007_).contents))
               then true
               else false
           | Some b_5 ->
               if
                 Ortac_runtime.Gospelstdlib.List.mem (a_4, b_5)
                   (Lazy.force new_state__007_).contents
               then true
               else false)
            = true
      | (Find_all a_5, Res ((List (Int), _), bs)) ->
          (Ortac_runtime.Gospelstdlib.List.to_seq bs) =
            (Ortac_runtime.Gospelstdlib.Sequence.filter_map
               (fun x_3 ->
                  if (Ortac_runtime.Gospelstdlib.fst x_3) = a_5
                  then Some (Ortac_runtime.Gospelstdlib.snd x_3)
                  else None)
               (Ortac_runtime.Gospelstdlib.List.to_seq
                  (Lazy.force new_state__007_).contents))
      | (Mem a_6, Res ((Bool, _), b_6)) ->
          (b_6 = true) =
            (Ortac_runtime.Gospelstdlib.List.mem a_6
               (Ortac_runtime.Gospelstdlib.List.map
                  (fun x_4 -> Ortac_runtime.Gospelstdlib.fst x_4)
                  (Lazy.force new_state__007_).contents))
      | (Remove a_7, Res ((Unit, _), _)) -> true
      | (Replace (a_8, b_3), Res ((Unit, _), _)) -> true
      | (Length, Res ((Int, _), i)) ->
          (Ortac_runtime.Gospelstdlib.integer_of_int i) =
            (Ortac_runtime.Gospelstdlib.List.length
               (Lazy.force new_state__007_).contents)
      | _ -> true
    let run cmd__010_ sut__011_ =
      match cmd__010_ with
      | Clear -> Res (unit, (clear sut__011_))
      | Reset -> Res (unit, (reset sut__011_))
      | Add (a_2, b_2) -> Res (unit, (add sut__011_ a_2 b_2))
      | Find a_3 ->
          Res ((result int exn), (protect (fun () -> find sut__011_ a_3) ()))
      | Find_opt a_4 -> Res ((option int), (find_opt sut__011_ a_4))
      | Find_all a_5 -> Res ((list int), (find_all sut__011_ a_5))
      | Mem a_6 -> Res (bool, (mem sut__011_ a_6))
      | Remove a_7 -> Res (unit, (remove sut__011_ a_7))
      | Replace (a_8, b_3) -> Res (unit, (replace sut__011_ a_8 b_3))
      | Length -> Res (int, (length sut__011_))
  end
module STMTests = (STM_sequential.Make)(Spec)
let _ =
  QCheck_base_runner.run_tests_main
    (let count = 1000 in
     [STMTests.agree_test ~count ~name:"STM Lib test sequential"])

open Hashtbl
module Spec =
  struct
    open STM
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
      | Filter_map_inplace of (char -> int -> int option) 
      | Length [@@deriving show { with_path = false }]
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
               ((pure (fun a_1 -> fun b_1 -> Add (a_1, b_1))) <*> char) <*>
                 int;
               (pure (fun a_2 -> Find a_2)) <*> char;
               (pure (fun a_3 -> Find_opt a_3)) <*> char;
               (pure (fun a_4 -> Find_all a_4)) <*> char;
               (pure (fun a_5 -> Mem a_5)) <*> char;
               (pure (fun a_6 -> Remove a_6)) <*> char;
               ((pure (fun a_7 -> fun b_2 -> Replace (a_7, b_2))) <*> char)
                 <*> int;
               pure Length])
    let next_state cmd__001_ state__002_ =
      match cmd__001_ with
      | Clear -> { state__002_ with contents = [] }
      | Reset -> { state__002_ with contents = [] }
      | Add (a_1, b_1) ->
          { state__002_ with contents = ((a_1, b_1) :: state__002_.contents)
          }
      | Find a_2 -> state__002_
      | Find_opt a_3 -> state__002_
      | Find_all a_4 -> state__002_
      | Mem a_5 -> state__002_
      | Remove a_6 -> state__002_
      | Replace (a_7, b_2) -> state__002_
      | Filter_map_inplace f -> state__002_
      | Length -> state__002_
    let precond cmd__007_ state__008_ =
      match cmd__007_ with
      | Clear -> true
      | Reset -> true
      | Add (a_1, b_1) -> true
      | Find a_2 -> true
      | Find_opt a_3 -> true
      | Find_all a_4 -> true
      | Mem a_5 -> true
      | Remove a_6 -> true
      | Replace (a_7, b_2) -> true
      | Filter_map_inplace f -> true
      | Length -> true
    let postcond cmd__003_ state__004_ res__005_ =
      let new_state__006_ = lazy (next_state cmd__003_ state__004_) in
      match (cmd__003_, res__005_) with
      | (Clear, Res ((Unit, _), _)) -> true
      | (Reset, Res ((Unit, _), _)) -> true
      | (Add (a_1, b_1), Res ((Unit, _), _)) -> true
      | (Find a_2, Res ((Result (Int, Exn), _), b_3)) ->
          (match b_3 with
           | Ok b_3 ->
               Ortac_runtime.Gospelstdlib.List.mem (a_2, b_3)
                 (Lazy.force new_state__006_).contents
           | Error (Not_found) ->
               not
                 (Ortac_runtime.Gospelstdlib.List.mem a_2
                    (Ortac_runtime.Gospelstdlib.List.map
                       (fun x -> Ortac_runtime.Gospelstdlib.fst x)
                       (Lazy.force new_state__006_).contents))
           | _ -> false)
      | (Find_opt a_3, Res ((Option (Int), _), o)) ->
          (match o with
           | None ->
               if
                 not
                   (Ortac_runtime.Gospelstdlib.List.mem a_3
                      (Ortac_runtime.Gospelstdlib.List.map
                         (fun x_1 -> Ortac_runtime.Gospelstdlib.fst x_1)
                         (Lazy.force new_state__006_).contents))
               then true
               else false
           | Some b_4 ->
               if
                 Ortac_runtime.Gospelstdlib.List.mem (a_3, b_4)
                   (Lazy.force new_state__006_).contents
               then true
               else false)
            = true
      | (Find_all a_4, Res ((List (Int), _), bs)) ->
          (Ortac_runtime.Gospelstdlib.List.to_seq bs) =
            (Ortac_runtime.Gospelstdlib.Sequence.filter_map
               (fun x_2 ->
                  if (Ortac_runtime.Gospelstdlib.fst x_2) = a_4
                  then Some (Ortac_runtime.Gospelstdlib.snd x_2)
                  else None)
               (Ortac_runtime.Gospelstdlib.List.to_seq
                  (Lazy.force new_state__006_).contents))
      | (Mem a_5, Res ((Bool, _), b_5)) ->
          (b_5 = true) =
            (Ortac_runtime.Gospelstdlib.List.mem a_5
               (Ortac_runtime.Gospelstdlib.List.map
                  (fun x_3 -> Ortac_runtime.Gospelstdlib.fst x_3)
                  (Lazy.force new_state__006_).contents))
      | (Remove a_6, Res ((Unit, _), _)) -> true
      | (Replace (a_7, b_2), Res ((Unit, _), _)) -> true
      | (Filter_map_inplace f, Res ((Unit, _), _)) ->
          (Ortac_runtime.Gospelstdlib.List.to_seq
             (Lazy.force new_state__006_).contents)
            =
            (Ortac_runtime.Gospelstdlib.Sequence.filter_map
               (fun x_4 ->
                  match f (Ortac_runtime.Gospelstdlib.fst x_4)
                          (Ortac_runtime.Gospelstdlib.snd x_4)
                  with
                  | None -> None
                  | Some b' ->
                      Some ((Ortac_runtime.Gospelstdlib.fst x_4), b'))
               (Ortac_runtime.Gospelstdlib.List.to_seq state__004_.contents))
      | (Length, Res ((Int, _), i)) ->
          (Ortac_runtime.Gospelstdlib.integer_of_int i) =
            (Ortac_runtime.Gospelstdlib.List.length
               (Lazy.force new_state__006_).contents)
      | _ -> true
    let run cmd__009_ sut__010_ =
      match cmd__009_ with
      | Clear -> Res (unit, (clear sut__010_))
      | Reset -> Res (unit, (reset sut__010_))
      | Add (a_1, b_1) -> Res (unit, (add sut__010_ a_1 b_1))
      | Find a_2 ->
          Res ((result int exn), (protect (fun () -> find sut__010_ a_2) ()))
      | Find_opt a_3 -> Res ((option int), (find_opt sut__010_ a_3))
      | Find_all a_4 -> Res ((list int), (find_all sut__010_ a_4))
      | Mem a_5 -> Res (bool, (mem sut__010_ a_5))
      | Remove a_6 -> Res (unit, (remove sut__010_ a_6))
      | Replace (a_7, b_2) -> Res (unit, (replace sut__010_ a_7 b_2))
      | Filter_map_inplace f -> Res (unit, (filter_map_inplace f sut__010_))
      | Length -> Res (int, (length sut__010_))
  end
module STMTests = (STM_sequential.Make)(Spec)
let _ =
  QCheck_base_runner.run_tests_main
    (let count = 1000 in
     [STMTests.agree_test ~count ~name:"STM Lib test sequential"])

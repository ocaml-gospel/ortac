open Array
module Spec =
  struct
    open STM
    type sut = char t
    type cmd =
      | Length 
      | Get of int 
      | Set of int * char 
      | Fill of int * int * char 
      | To_list 
      | Mem of char [@@deriving show { with_path = false }]
    type nonrec state = {
      size: int ;
      contents: char list }
    let init_state =
      let i_4 = 16
      and a_5 = 'a' in
      {
        size = i_4;
        contents =
          (Ortac_runtime.Gospelstdlib.List.init
             (Ortac_runtime.Gospelstdlib.integer_of_int i_4) (fun j_2 -> a_5))
      }
    let init_sut () = make 16 'a'
    let cleanup _ = ()
    let arb_cmd _ =
      let open QCheck in
        make ~print:show_cmd
          (let open Gen in
             oneof
               [pure Length;
               (pure (fun i -> Get i)) <*> int;
               ((pure (fun i_1 -> fun a_1 -> Set (i_1, a_1))) <*> int) <*>
                 char;
               (((pure
                    (fun i_2 -> fun j_1 -> fun a_2 -> Fill (i_2, j_1, a_2)))
                   <*> int)
                  <*> int)
                 <*> char;
               pure To_list;
               (pure (fun a_3 -> Mem a_3)) <*> char])
    let next_state cmd__001_ state__002_ =
      match cmd__001_ with
      | Length -> state__002_
      | Get i -> state__002_
      | Set (i_1, a_1) ->
          if
            let __t1__003_ =
              Ortac_runtime.Gospelstdlib.(<=)
                (Ortac_runtime.Gospelstdlib.integer_of_int 0)
                (Ortac_runtime.Gospelstdlib.integer_of_int i_1) in
            let __t2__004_ =
              Ortac_runtime.Gospelstdlib.(<)
                (Ortac_runtime.Gospelstdlib.integer_of_int i_1)
                (Ortac_runtime.Gospelstdlib.integer_of_int state__002_.size) in
            __t1__003_ && __t2__004_
          then
            {
              state__002_ with
              contents =
                (Ortac_runtime.Gospelstdlib.List.mapi
                   (fun j ->
                      fun x ->
                        if
                          j = (Ortac_runtime.Gospelstdlib.integer_of_int i_1)
                        then a_1
                        else x) state__002_.contents)
            }
          else state__002_
      | Fill (i_2, j_1, a_2) ->
          if
            (Ortac_runtime.Gospelstdlib.(<=)
               (Ortac_runtime.Gospelstdlib.integer_of_int 0)
               (Ortac_runtime.Gospelstdlib.integer_of_int i_2))
              &&
              ((Ortac_runtime.Gospelstdlib.(<=)
                  (Ortac_runtime.Gospelstdlib.integer_of_int 0)
                  (Ortac_runtime.Gospelstdlib.integer_of_int j_1))
                 &&
                 (Ortac_runtime.Gospelstdlib.(<=)
                    (Ortac_runtime.Gospelstdlib.(+)
                       (Ortac_runtime.Gospelstdlib.integer_of_int i_2)
                       (Ortac_runtime.Gospelstdlib.integer_of_int j_1))
                    (Ortac_runtime.Gospelstdlib.integer_of_int
                       state__002_.size)))
          then
            {
              state__002_ with
              contents =
                (Ortac_runtime.Gospelstdlib.List.mapi
                   (fun k ->
                      fun x_1 ->
                        if
                          let __t1__005_ =
                            Ortac_runtime.Gospelstdlib.(<=)
                              (Ortac_runtime.Gospelstdlib.integer_of_int i_2)
                              k in
                          let __t2__006_ =
                            Ortac_runtime.Gospelstdlib.(<) k
                              (Ortac_runtime.Gospelstdlib.(+)
                                 (Ortac_runtime.Gospelstdlib.integer_of_int
                                    i_2)
                                 (Ortac_runtime.Gospelstdlib.integer_of_int
                                    j_1)) in
                          __t1__005_ && __t2__006_
                        then a_2
                        else x_1) state__002_.contents)
            }
          else state__002_
      | To_list -> state__002_
      | Mem a_3 -> state__002_
    let precond cmd__015_ state__016_ =
      match cmd__015_ with
      | Length -> true
      | Get i -> true
      | Set (i_1, a_1) -> true
      | Fill (i_2, j_1, a_2) -> true
      | To_list -> true
      | Mem a_3 -> true
    let postcond cmd__007_ state__008_ res__009_ =
      let new_state__010_ = lazy (next_state cmd__007_ state__008_) in
      match (cmd__007_, res__009_) with
      | (Length, Res ((Int, _), i_3)) ->
          i_3 = (Lazy.force new_state__010_).size
      | (Get i, Res ((Result (Char, Exn), _), a_4)) ->
          if
            let __t1__011_ =
              Ortac_runtime.Gospelstdlib.(<=)
                (Ortac_runtime.Gospelstdlib.integer_of_int 0)
                (Ortac_runtime.Gospelstdlib.integer_of_int i) in
            let __t2__012_ =
              Ortac_runtime.Gospelstdlib.(<)
                (Ortac_runtime.Gospelstdlib.integer_of_int i)
                (Ortac_runtime.Gospelstdlib.integer_of_int state__008_.size) in
            __t1__011_ && __t2__012_
          then
            (match a_4 with
             | Ok a_4 ->
                 a_4 =
                   (Ortac_runtime.Gospelstdlib.List.nth
                      (Lazy.force new_state__010_).contents
                      (Ortac_runtime.Gospelstdlib.integer_of_int i))
             | _ -> false)
          else
            (match a_4 with | Error (Invalid_argument _) -> true | _ -> false)
      | (Set (i_1, a_1), Res ((Result (Unit, Exn), _), res)) ->
          if
            let __t1__013_ =
              Ortac_runtime.Gospelstdlib.(<=)
                (Ortac_runtime.Gospelstdlib.integer_of_int 0)
                (Ortac_runtime.Gospelstdlib.integer_of_int i_1) in
            let __t2__014_ =
              Ortac_runtime.Gospelstdlib.(<)
                (Ortac_runtime.Gospelstdlib.integer_of_int i_1)
                (Ortac_runtime.Gospelstdlib.integer_of_int state__008_.size) in
            __t1__013_ && __t2__014_
          then (match res with | Ok _ -> true | _ -> false)
          else
            (match res with | Error (Invalid_argument _) -> true | _ -> false)
      | (Fill (i_2, j_1, a_2), Res ((Result (Unit, Exn), _), res)) ->
          if
            (Ortac_runtime.Gospelstdlib.(<=)
               (Ortac_runtime.Gospelstdlib.integer_of_int 0)
               (Ortac_runtime.Gospelstdlib.integer_of_int i_2))
              &&
              ((Ortac_runtime.Gospelstdlib.(<=)
                  (Ortac_runtime.Gospelstdlib.integer_of_int 0)
                  (Ortac_runtime.Gospelstdlib.integer_of_int j_1))
                 &&
                 (Ortac_runtime.Gospelstdlib.(<=)
                    (Ortac_runtime.Gospelstdlib.(+)
                       (Ortac_runtime.Gospelstdlib.integer_of_int i_2)
                       (Ortac_runtime.Gospelstdlib.integer_of_int j_1))
                    (Ortac_runtime.Gospelstdlib.integer_of_int
                       state__008_.size)))
          then (match res with | Ok _ -> true | _ -> false)
          else
            (match res with | Error (Invalid_argument _) -> true | _ -> false)
      | (To_list, Res ((List (Char), _), l)) ->
          l = (Lazy.force new_state__010_).contents
      | (Mem a_3, Res ((Bool, _), b)) ->
          (b = true) =
            (Ortac_runtime.Gospelstdlib.List.mem a_3
               (Lazy.force new_state__010_).contents)
      | _ -> true
    let run cmd__017_ sut__018_ =
      match cmd__017_ with
      | Length -> Res (int, (length sut__018_))
      | Get i ->
          Res ((result char exn), (protect (fun () -> get sut__018_ i) ()))
      | Set (i_1, a_1) ->
          Res
            ((result unit exn),
              (protect (fun () -> set sut__018_ i_1 a_1) ()))
      | Fill (i_2, j_1, a_2) ->
          Res
            ((result unit exn),
              (protect (fun () -> fill sut__018_ i_2 j_1 a_2) ()))
      | To_list -> Res ((list char), (to_list sut__018_))
      | Mem a_3 -> Res (bool, (mem a_3 sut__018_))
  end
module STMTests = (STM_sequential.Make)(Spec)
let _ =
  QCheck_base_runner.run_tests_main
    (let count = 1000 in
     [STMTests.agree_test ~count ~name:"STM Lib test sequential"])

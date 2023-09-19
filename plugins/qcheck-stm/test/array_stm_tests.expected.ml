open Array
module Spec =
  struct
    open STM
    [@@@ocaml.warning "-26-27"]
    type sut = char t
    type cmd =
      | Length 
      | Get of int 
      | Set of int * char 
      | Fill of int * int * char 
      | To_list 
      | Mem of char 
    let show_cmd cmd__001_ =
      match cmd__001_ with
      | Length -> Format.asprintf "%s" "length"
      | Get i -> Format.asprintf "%s %a" "get" (Util.Pp.pp_int true) i
      | Set (i_1, a_1) ->
          Format.asprintf "%s %a %a" "set" (Util.Pp.pp_int true) i_1
            (Util.Pp.pp_char true) a_1
      | Fill (i_2, j, a_2) ->
          Format.asprintf "%s %a %a %a" "fill" (Util.Pp.pp_int true) i_2
            (Util.Pp.pp_int true) j (Util.Pp.pp_char true) a_2
      | To_list -> Format.asprintf "%s" "to_list"
      | Mem a_3 -> Format.asprintf "%s %a" "mem" (Util.Pp.pp_char true) a_3
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
               (((pure (fun i_2 -> fun j -> fun a_2 -> Fill (i_2, j, a_2)))
                   <*> int)
                  <*> int)
                 <*> char;
               pure To_list;
               (pure (fun a_3 -> Mem a_3)) <*> char])
    let next_state cmd__002_ state__003_ =
      match cmd__002_ with
      | Length -> state__003_
      | Get i -> state__003_
      | Set (i_1, a_1) ->
          if
            let __t1__004_ =
              Ortac_runtime.Gospelstdlib.(<=)
                (Ortac_runtime.Gospelstdlib.integer_of_int 0)
                (Ortac_runtime.Gospelstdlib.integer_of_int i_1) in
            let __t2__005_ =
              Ortac_runtime.Gospelstdlib.(<)
                (Ortac_runtime.Gospelstdlib.integer_of_int i_1)
                (Ortac_runtime.Gospelstdlib.integer_of_int state__003_.size) in
            __t1__004_ && __t2__005_
          then
            {
              state__003_ with
              contents =
                (Ortac_runtime.Gospelstdlib.List.mapi
                   (fun j_1 ->
                      fun x ->
                        if
                          j_1 =
                            (Ortac_runtime.Gospelstdlib.integer_of_int i_1)
                        then a_1
                        else x) state__003_.contents)
            }
          else state__003_
      | Fill (i_2, j, a_2) ->
          if
            (Ortac_runtime.Gospelstdlib.(<=)
               (Ortac_runtime.Gospelstdlib.integer_of_int 0)
               (Ortac_runtime.Gospelstdlib.integer_of_int i_2))
              &&
              ((Ortac_runtime.Gospelstdlib.(<=)
                  (Ortac_runtime.Gospelstdlib.integer_of_int 0)
                  (Ortac_runtime.Gospelstdlib.integer_of_int j))
                 &&
                 (Ortac_runtime.Gospelstdlib.(<=)
                    (Ortac_runtime.Gospelstdlib.(+)
                       (Ortac_runtime.Gospelstdlib.integer_of_int i_2)
                       (Ortac_runtime.Gospelstdlib.integer_of_int j))
                    (Ortac_runtime.Gospelstdlib.integer_of_int
                       state__003_.size)))
          then
            {
              state__003_ with
              contents =
                (Ortac_runtime.Gospelstdlib.List.mapi
                   (fun k ->
                      fun x_1 ->
                        if
                          let __t1__006_ =
                            Ortac_runtime.Gospelstdlib.(<=)
                              (Ortac_runtime.Gospelstdlib.integer_of_int i_2)
                              k in
                          let __t2__007_ =
                            Ortac_runtime.Gospelstdlib.(<) k
                              (Ortac_runtime.Gospelstdlib.(+)
                                 (Ortac_runtime.Gospelstdlib.integer_of_int
                                    i_2)
                                 (Ortac_runtime.Gospelstdlib.integer_of_int j)) in
                          __t1__006_ && __t2__007_
                        then a_2
                        else x_1) state__003_.contents)
            }
          else state__003_
      | To_list -> state__003_
      | Mem a_3 -> state__003_
    let precond cmd__016_ state__017_ =
      match cmd__016_ with
      | Length -> true
      | Get i -> true
      | Set (i_1, a_1) -> true
      | Fill (i_2, j, a_2) -> true
      | To_list -> true
      | Mem a_3 -> true
    let postcond cmd__008_ state__009_ res__010_ =
      let new_state__011_ = lazy (next_state cmd__008_ state__009_) in
      match (cmd__008_, res__010_) with
      | (Length, Res ((Int, _), i_3)) ->
          i_3 = (Lazy.force new_state__011_).size
      | (Get i, Res ((Result (Char, Exn), _), a_4)) ->
          if
            let __t1__012_ =
              Ortac_runtime.Gospelstdlib.(<=)
                (Ortac_runtime.Gospelstdlib.integer_of_int 0)
                (Ortac_runtime.Gospelstdlib.integer_of_int i) in
            let __t2__013_ =
              Ortac_runtime.Gospelstdlib.(<)
                (Ortac_runtime.Gospelstdlib.integer_of_int i)
                (Ortac_runtime.Gospelstdlib.integer_of_int state__009_.size) in
            __t1__012_ && __t2__013_
          then
            (match a_4 with
             | Ok a_4 ->
                 a_4 =
                   (Ortac_runtime.Gospelstdlib.List.nth
                      (Lazy.force new_state__011_).contents
                      (Ortac_runtime.Gospelstdlib.integer_of_int i))
             | _ -> false)
          else
            (match a_4 with | Error (Invalid_argument _) -> true | _ -> false)
      | (Set (i_1, a_1), Res ((Result (Unit, Exn), _), res)) ->
          if
            let __t1__014_ =
              Ortac_runtime.Gospelstdlib.(<=)
                (Ortac_runtime.Gospelstdlib.integer_of_int 0)
                (Ortac_runtime.Gospelstdlib.integer_of_int i_1) in
            let __t2__015_ =
              Ortac_runtime.Gospelstdlib.(<)
                (Ortac_runtime.Gospelstdlib.integer_of_int i_1)
                (Ortac_runtime.Gospelstdlib.integer_of_int state__009_.size) in
            __t1__014_ && __t2__015_
          then (match res with | Ok _ -> true | _ -> false)
          else
            (match res with | Error (Invalid_argument _) -> true | _ -> false)
      | (Fill (i_2, j, a_2), Res ((Result (Unit, Exn), _), res)) ->
          if
            (Ortac_runtime.Gospelstdlib.(<=)
               (Ortac_runtime.Gospelstdlib.integer_of_int 0)
               (Ortac_runtime.Gospelstdlib.integer_of_int i_2))
              &&
              ((Ortac_runtime.Gospelstdlib.(<=)
                  (Ortac_runtime.Gospelstdlib.integer_of_int 0)
                  (Ortac_runtime.Gospelstdlib.integer_of_int j))
                 &&
                 (Ortac_runtime.Gospelstdlib.(<=)
                    (Ortac_runtime.Gospelstdlib.(+)
                       (Ortac_runtime.Gospelstdlib.integer_of_int i_2)
                       (Ortac_runtime.Gospelstdlib.integer_of_int j))
                    (Ortac_runtime.Gospelstdlib.integer_of_int
                       state__009_.size)))
          then (match res with | Ok _ -> true | _ -> false)
          else
            (match res with | Error (Invalid_argument _) -> true | _ -> false)
      | (To_list, Res ((List (Char), _), l)) ->
          l = (Lazy.force new_state__011_).contents
      | (Mem a_3, Res ((Bool, _), b)) ->
          (b = true) =
            (Ortac_runtime.Gospelstdlib.List.mem a_3
               (Lazy.force new_state__011_).contents)
      | _ -> true
    let run cmd__018_ sut__019_ =
      match cmd__018_ with
      | Length -> Res (int, (length sut__019_))
      | Get i ->
          Res ((result char exn), (protect (fun () -> get sut__019_ i) ()))
      | Set (i_1, a_1) ->
          Res
            ((result unit exn),
              (protect (fun () -> set sut__019_ i_1 a_1) ()))
      | Fill (i_2, j, a_2) ->
          Res
            ((result unit exn),
              (protect (fun () -> fill sut__019_ i_2 j a_2) ()))
      | To_list -> Res ((list char), (to_list sut__019_))
      | Mem a_3 -> Res (bool, (mem a_3 sut__019_))
  end
module STMTests = (STM_sequential.Make)(Spec)
let _ =
  QCheck_base_runner.run_tests_main
    (let count = 1000 in
     [STMTests.agree_test ~count ~name:"STM Lib test sequential"])

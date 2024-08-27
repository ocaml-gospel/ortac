(* This file is generated by ortac qcheck-stm,
   edit how you run the tool instead *)
[@@@ocaml.warning "-26-27-69-32"]
open Queue
module Ortac_runtime = Ortac_runtime_qcheck_stm
module SUT =
  (Ortac_runtime.SUT.Make)(struct type sut = int t
                                  let init () = create () end)
module ModelElt =
  struct
    type nonrec elt = {
      contents: int Ortac_runtime.Gospelstdlib.sequence }
    let init =
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
                            pos_fname = "queue.mli";
                            pos_lnum = 8;
                            pos_bol = 244;
                            pos_cnum = 269
                          };
                        Ortac_runtime.stop =
                          {
                            pos_fname = "queue.mli";
                            pos_lnum = 8;
                            pos_bol = 244;
                            pos_cnum = 283
                          }
                      })))
      }
  end
module Model = (Ortac_runtime.Model.Make)(ModelElt)
module Spec =
  struct
    open STM
    module QCheck =
      struct
        include QCheck
        module Gen = struct include Gen
                            let int = small_signed_int end
      end
    type _ ty +=  
      | Integer: Ortac_runtime.integer ty 
    let integer = (Integer, Ortac_runtime.string_of_integer)
    type sut = SUT.t
    let init_sut = SUT.create 2
    type state = Model.t
    let init_state = Model.create 2 ()
    type cmd =
      | Push of int 
      | Pop 
      | Peek 
      | Peek_opt 
      | Clear 
      | Is_empty 
      | Transfer 
    let show_cmd cmd__001_ =
      match cmd__001_ with
      | Push v ->
          Format.asprintf "%s %a <sut>" "push" (Util.Pp.pp_int true) v
      | Pop -> Format.asprintf "protect (fun () -> %s <sut>)" "pop"
      | Peek -> Format.asprintf "protect (fun () -> %s <sut>)" "peek"
      | Peek_opt -> Format.asprintf "%s <sut>" "peek_opt"
      | Clear -> Format.asprintf "%s <sut>" "clear"
      | Is_empty -> Format.asprintf "%s <sut>" "is_empty"
      | Transfer -> Format.asprintf "%s <sut> <sut>" "transfer"
    let cleanup _ = ()
    let arb_cmd _ =
      let open QCheck in
        make ~print:show_cmd
          (let open Gen in
             oneof
               [(pure (fun v -> Push v)) <*> int;
               pure Pop;
               pure Peek;
               pure Peek_opt;
               pure Clear;
               pure Is_empty;
               pure Transfer])
    let next_state cmd__002_ state__003_ =
      match cmd__002_ with
      | Push v ->
          let t_1__004_ = Model.get state__003_ 0 in
          let t_1__005_ =
            let open ModelElt in
              {
                contents =
                  (try
                     Ortac_runtime.Gospelstdlib.Sequence.snoc
                       t_1__004_.contents v
                   with
                   | e ->
                       raise
                         (Ortac_runtime.Partial_function
                            (e,
                              {
                                Ortac_runtime.start =
                                  {
                                    pos_fname = "queue.mli";
                                    pos_lnum = 13;
                                    pos_bol = 441;
                                    pos_cnum = 466
                                  };
                                Ortac_runtime.stop =
                                  {
                                    pos_fname = "queue.mli";
                                    pos_lnum = 13;
                                    pos_bol = 441;
                                    pos_cnum = 498
                                  }
                              })))
              } in
          Model.push (Model.drop_n state__003_ 1) t_1__005_
      | Pop ->
          let t_2__006_ = Model.get state__003_ 0 in
          let t_2__008_ =
            let open ModelElt in
              {
                contents =
                  (try
                     match Ortac_runtime.Gospelstdlib.Sequence.length
                             t_2__006_.contents
                     with
                     | __x__007_ when
                         (=) __x__007_
                           (Ortac_runtime.Gospelstdlib.integer_of_int 0)
                         -> Ortac_runtime.Gospelstdlib.Sequence.empty
                     | _ ->
                         Ortac_runtime.Gospelstdlib.Sequence.tl
                           t_2__006_.contents
                   with
                   | e ->
                       raise
                         (Ortac_runtime.Partial_function
                            (e,
                              {
                                Ortac_runtime.start =
                                  {
                                    pos_fname = "queue.mli";
                                    pos_lnum = 19;
                                    pos_bol = 732;
                                    pos_cnum = 757
                                  };
                                Ortac_runtime.stop =
                                  {
                                    pos_fname = "queue.mli";
                                    pos_lnum = 21;
                                    pos_bol = 831;
                                    pos_cnum = 874
                                  }
                              })))
              } in
          Model.push (Model.drop_n state__003_ 1) t_2__008_
      | Peek ->
          let t_3__009_ = Model.get state__003_ 0 in
          let t_3__010_ = t_3__009_ in
          Model.push (Model.drop_n state__003_ 1) t_3__010_
      | Peek_opt ->
          let t_4__011_ = Model.get state__003_ 0 in
          let t_4__012_ = t_4__011_ in
          Model.push (Model.drop_n state__003_ 1) t_4__012_
      | Clear ->
          let t_5__013_ = Model.get state__003_ 0 in
          let t_5__014_ =
            let open ModelElt in
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
                                    pos_fname = "queue.mli";
                                    pos_lnum = 39;
                                    pos_bol = 1621;
                                    pos_cnum = 1646
                                  };
                                Ortac_runtime.stop =
                                  {
                                    pos_fname = "queue.mli";
                                    pos_lnum = 39;
                                    pos_bol = 1621;
                                    pos_cnum = 1660
                                  }
                              })))
              } in
          Model.push (Model.drop_n state__003_ 1) t_5__014_
      | Is_empty ->
          let t_6__015_ = Model.get state__003_ 0 in
          let t_6__016_ = t_6__015_ in
          Model.push (Model.drop_n state__003_ 1) t_6__016_
      | Transfer ->
          let t1__017_ = Model.get state__003_ 0
          and t2__018_ = Model.get state__003_ 1 in
          let t1__020_ =
            let open ModelElt in
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
                                    pos_fname = "queue.mli";
                                    pos_lnum = 51;
                                    pos_bol = 2071;
                                    pos_cnum = 2097
                                  };
                                Ortac_runtime.stop =
                                  {
                                    pos_fname = "queue.mli";
                                    pos_lnum = 51;
                                    pos_bol = 2071;
                                    pos_cnum = 2111
                                  }
                              })))
              }
          and t2__019_ =
            let open ModelElt in
              {
                contents =
                  (try
                     Ortac_runtime.Gospelstdlib.Sequence.append
                       t1__017_.contents t2__018_.contents
                   with
                   | e ->
                       raise
                         (Ortac_runtime.Partial_function
                            (e,
                              {
                                Ortac_runtime.start =
                                  {
                                    pos_fname = "queue.mli";
                                    pos_lnum = 52;
                                    pos_bol = 2112;
                                    pos_cnum = 2138
                                  };
                                Ortac_runtime.stop =
                                  {
                                    pos_fname = "queue.mli";
                                    pos_lnum = 52;
                                    pos_bol = 2112;
                                    pos_cnum = 2189
                                  }
                              })))
              } in
          Model.push (Model.push (Model.drop_n state__003_ 2) t2__019_)
            t1__020_
    let precond cmd__059_ state__060_ =
      match cmd__059_ with
      | Push v -> let t_1__061_ = Model.get state__060_ 0 in true
      | Pop -> let t_2__062_ = Model.get state__060_ 0 in true
      | Peek -> let t_3__063_ = Model.get state__060_ 0 in true
      | Peek_opt -> let t_4__064_ = Model.get state__060_ 0 in true
      | Clear -> let t_5__065_ = Model.get state__060_ 0 in true
      | Is_empty -> let t_6__066_ = Model.get state__060_ 0 in true
      | Transfer ->
          let t1__067_ = Model.get state__060_ 0
          and t2__068_ = Model.get state__060_ 1 in true
    let postcond _ _ _ = true
    let run cmd__069_ sut__070_ =
      match cmd__069_ with
      | Push v ->
          Res
            (unit,
              (let t_1__071_ = SUT.pop sut__070_ in
               let res__072_ = push v t_1__071_ in
               (SUT.push sut__070_ t_1__071_; res__072_)))
      | Pop ->
          Res
            ((result int exn),
              (let t_2__073_ = SUT.pop sut__070_ in
               let res__074_ = protect (fun () -> pop t_2__073_) () in
               (SUT.push sut__070_ t_2__073_; res__074_)))
      | Peek ->
          Res
            ((result int exn),
              (let t_3__075_ = SUT.pop sut__070_ in
               let res__076_ = protect (fun () -> peek t_3__075_) () in
               (SUT.push sut__070_ t_3__075_; res__076_)))
      | Peek_opt ->
          Res
            ((option int),
              (let t_4__077_ = SUT.pop sut__070_ in
               let res__078_ = peek_opt t_4__077_ in
               (SUT.push sut__070_ t_4__077_; res__078_)))
      | Clear ->
          Res
            (unit,
              (let t_5__079_ = SUT.pop sut__070_ in
               let res__080_ = clear t_5__079_ in
               (SUT.push sut__070_ t_5__079_; res__080_)))
      | Is_empty ->
          Res
            (bool,
              (let t_6__081_ = SUT.pop sut__070_ in
               let res__082_ = is_empty t_6__081_ in
               (SUT.push sut__070_ t_6__081_; res__082_)))
      | Transfer ->
          Res
            (unit,
              (let t1__083_ = SUT.pop sut__070_ in
               let t2__084_ = SUT.pop sut__070_ in
               let res__085_ = transfer t1__083_ t2__084_ in
               (SUT.push sut__070_ t2__084_;
                SUT.push sut__070_ t1__083_;
                res__085_)))
  end
module STMTests = (Ortac_runtime.Make)(Spec)
let check_init_state () = ()
let ortac_postcond cmd__021_ state__022_ res__023_ =
  let open Spec in
    let open STM in
      let new_state__024_ = lazy (next_state cmd__021_ state__022_) in
      match (cmd__021_, res__023_) with
      | (Push v, Res ((Unit, _), _)) -> None
      | (Pop, Res ((Result (Int, Exn), _), v_1)) ->
          (match v_1 with
           | Ok v_1 ->
               Ortac_runtime.append
                 (if
                    let t_old__028_ = Model.get state__022_ 0
                    and t_new__029_ =
                      lazy (Model.get (Lazy.force new_state__024_) 0) in
                    try
                      v_1 =
                        (Ortac_runtime.Gospelstdlib.Sequence.hd
                           t_old__028_.contents)
                    with
                    | e ->
                        raise
                          (Ortac_runtime.Partial_function
                             (e,
                               {
                                 Ortac_runtime.start =
                                   {
                                     pos_fname = "queue.mli";
                                     pos_lnum = 22;
                                     pos_bol = 875;
                                     pos_cnum = 887
                                   };
                                 Ortac_runtime.stop =
                                   {
                                     pos_fname = "queue.mli";
                                     pos_lnum = 22;
                                     pos_bol = 875;
                                     pos_cnum = 919
                                   }
                               }))
                  then None
                  else
                    Some
                      (Ortac_runtime.report "Queue" "create ()"
                         (Either.right
                            (Res
                               (int,
                                 (let t_old__026_ = Model.get state__022_ 0
                                  and t_new__027_ =
                                    lazy
                                      (Model.get (Lazy.force new_state__024_)
                                         0) in
                                  try
                                    Ortac_runtime.Gospelstdlib.Sequence.hd
                                      t_old__026_.contents
                                  with
                                  | e ->
                                      raise
                                        (Ortac_runtime.Partial_function
                                           (e,
                                             {
                                               Ortac_runtime.start =
                                                 {
                                                   pos_fname = "queue.mli";
                                                   pos_lnum = 22;
                                                   pos_bol = 875;
                                                   pos_cnum = 891
                                                 };
                                               Ortac_runtime.stop =
                                                 {
                                                   pos_fname = "queue.mli";
                                                   pos_lnum = 22;
                                                   pos_bol = 875;
                                                   pos_cnum = 919
                                                 }
                                             })))))) "pop"
                         [("v = Sequence.hd (old t.contents)",
                            {
                              Ortac_runtime.start =
                                {
                                  pos_fname = "queue.mli";
                                  pos_lnum = 22;
                                  pos_bol = 875;
                                  pos_cnum = 887
                                };
                              Ortac_runtime.stop =
                                {
                                  pos_fname = "queue.mli";
                                  pos_lnum = 22;
                                  pos_bol = 875;
                                  pos_cnum = 919
                                }
                            })]))
                 (if
                    let t_old__030_ = Model.get state__022_ 0
                    and t_new__031_ =
                      lazy (Model.get (Lazy.force new_state__024_) 0) in
                    try
                      not
                        (t_old__030_.contents =
                           Ortac_runtime.Gospelstdlib.Sequence.empty)
                    with
                    | e ->
                        raise
                          (Ortac_runtime.Partial_function
                             (e,
                               {
                                 Ortac_runtime.start =
                                   {
                                     pos_fname = "queue.mli";
                                     pos_lnum = 23;
                                     pos_bol = 920;
                                     pos_cnum = 932
                                   };
                                 Ortac_runtime.stop =
                                   {
                                     pos_fname = "queue.mli";
                                     pos_lnum = 23;
                                     pos_bol = 920;
                                     pos_cnum = 964
                                   }
                               }))
                  then None
                  else
                    Some
                      (Ortac_runtime.report "Queue" "create ()"
                         (Either.right
                            (Res
                               (int,
                                 (let t_old__026_ = Model.get state__022_ 0
                                  and t_new__027_ =
                                    lazy
                                      (Model.get (Lazy.force new_state__024_)
                                         0) in
                                  try
                                    Ortac_runtime.Gospelstdlib.Sequence.hd
                                      t_old__026_.contents
                                  with
                                  | e ->
                                      raise
                                        (Ortac_runtime.Partial_function
                                           (e,
                                             {
                                               Ortac_runtime.start =
                                                 {
                                                   pos_fname = "queue.mli";
                                                   pos_lnum = 22;
                                                   pos_bol = 875;
                                                   pos_cnum = 891
                                                 };
                                               Ortac_runtime.stop =
                                                 {
                                                   pos_fname = "queue.mli";
                                                   pos_lnum = 22;
                                                   pos_bol = 875;
                                                   pos_cnum = 919
                                                 }
                                             })))))) "pop"
                         [("old t.contents <> Sequence.empty",
                            {
                              Ortac_runtime.start =
                                {
                                  pos_fname = "queue.mli";
                                  pos_lnum = 23;
                                  pos_bol = 920;
                                  pos_cnum = 932
                                };
                              Ortac_runtime.stop =
                                {
                                  pos_fname = "queue.mli";
                                  pos_lnum = 23;
                                  pos_bol = 920;
                                  pos_cnum = 964
                                }
                            })]))
           | Error (Empty) ->
               if
                 let t_old__032_ = Model.get state__022_ 0
                 and t_new__033_ =
                   lazy (Model.get (Lazy.force new_state__024_) 0) in
                 (try
                    let __t1__034_ =
                      (Lazy.force t_new__033_).contents =
                        t_old__032_.contents in
                    let __t2__035_ =
                      t_old__032_.contents =
                        Ortac_runtime.Gospelstdlib.Sequence.empty in
                    __t1__034_ && __t2__035_
                  with
                  | e ->
                      raise
                        (Ortac_runtime.Partial_function
                           (e,
                             {
                               Ortac_runtime.start =
                                 {
                                   pos_fname = "queue.mli";
                                   pos_lnum = 17;
                                   pos_bol = 643;
                                   pos_cnum = 663
                                 };
                               Ortac_runtime.stop =
                                 {
                                   pos_fname = "queue.mli";
                                   pos_lnum = 17;
                                   pos_bol = 643;
                                   pos_cnum = 707
                                 }
                             })))
               then None
               else
                 Some
                   (Ortac_runtime.report "Queue" "create ()"
                      (Either.left "Empty") "pop"
                      [("t.contents = old t.contents = Sequence.empty",
                         {
                           Ortac_runtime.start =
                             {
                               pos_fname = "queue.mli";
                               pos_lnum = 17;
                               pos_bol = 643;
                               pos_cnum = 663
                             };
                           Ortac_runtime.stop =
                             {
                               pos_fname = "queue.mli";
                               pos_lnum = 17;
                               pos_bol = 643;
                               pos_cnum = 707
                             }
                         })])
           | _ -> None)
      | (Peek, Res ((Result (Int, Exn), _), v_2)) ->
          (match v_2 with
           | Ok v_2 ->
               if
                 let t_old__039_ = Model.get state__022_ 0
                 and t_new__040_ =
                   lazy (Model.get (Lazy.force new_state__024_) 0) in
                 (try
                    v_2 =
                      (Ortac_runtime.Gospelstdlib.Sequence.hd
                         (Lazy.force t_new__040_).contents)
                  with
                  | e ->
                      raise
                        (Ortac_runtime.Partial_function
                           (e,
                             {
                               Ortac_runtime.start =
                                 {
                                   pos_fname = "queue.mli";
                                   pos_lnum = 28;
                                   pos_bol = 1163;
                                   pos_cnum = 1175
                                 };
                               Ortac_runtime.stop =
                                 {
                                   pos_fname = "queue.mli";
                                   pos_lnum = 28;
                                   pos_bol = 1163;
                                   pos_cnum = 1201
                                 }
                             })))
               then None
               else
                 Some
                   (Ortac_runtime.report "Queue" "create ()"
                      (Either.right
                         (Res
                            (int,
                              (let t_old__037_ = Model.get state__022_ 0
                               and t_new__038_ =
                                 lazy
                                   (Model.get (Lazy.force new_state__024_) 0) in
                               try
                                 Ortac_runtime.Gospelstdlib.Sequence.hd
                                   (Lazy.force t_new__038_).contents
                               with
                               | e ->
                                   raise
                                     (Ortac_runtime.Partial_function
                                        (e,
                                          {
                                            Ortac_runtime.start =
                                              {
                                                pos_fname = "queue.mli";
                                                pos_lnum = 28;
                                                pos_bol = 1163;
                                                pos_cnum = 1179
                                              };
                                            Ortac_runtime.stop =
                                              {
                                                pos_fname = "queue.mli";
                                                pos_lnum = 28;
                                                pos_bol = 1163;
                                                pos_cnum = 1201
                                              }
                                          })))))) "peek"
                      [("v = Sequence.hd t.contents",
                         {
                           Ortac_runtime.start =
                             {
                               pos_fname = "queue.mli";
                               pos_lnum = 28;
                               pos_bol = 1163;
                               pos_cnum = 1175
                             };
                           Ortac_runtime.stop =
                             {
                               pos_fname = "queue.mli";
                               pos_lnum = 28;
                               pos_bol = 1163;
                               pos_cnum = 1201
                             }
                         })])
           | Error (Empty) ->
               if
                 let t_old__041_ = Model.get state__022_ 0
                 and t_new__042_ =
                   lazy (Model.get (Lazy.force new_state__024_) 0) in
                 (try
                    let __t1__043_ =
                      (Lazy.force t_new__042_).contents =
                        t_old__041_.contents in
                    let __t2__044_ =
                      t_old__041_.contents =
                        Ortac_runtime.Gospelstdlib.Sequence.empty in
                    __t1__043_ && __t2__044_
                  with
                  | e ->
                      raise
                        (Ortac_runtime.Partial_function
                           (e,
                             {
                               Ortac_runtime.start =
                                 {
                                   pos_fname = "queue.mli";
                                   pos_lnum = 27;
                                   pos_bol = 1098;
                                   pos_cnum = 1118
                                 };
                               Ortac_runtime.stop =
                                 {
                                   pos_fname = "queue.mli";
                                   pos_lnum = 27;
                                   pos_bol = 1098;
                                   pos_cnum = 1162
                                 }
                             })))
               then None
               else
                 Some
                   (Ortac_runtime.report "Queue" "create ()"
                      (Either.left "Empty") "peek"
                      [("t.contents = old t.contents = Sequence.empty",
                         {
                           Ortac_runtime.start =
                             {
                               pos_fname = "queue.mli";
                               pos_lnum = 27;
                               pos_bol = 1098;
                               pos_cnum = 1118
                             };
                           Ortac_runtime.stop =
                             {
                               pos_fname = "queue.mli";
                               pos_lnum = 27;
                               pos_bol = 1098;
                               pos_cnum = 1162
                             }
                         })])
           | _ -> None)
      | (Peek_opt, Res ((Option (Int), _), v_3)) ->
          if
            let t_old__046_ = Model.get state__022_ 0
            and t_new__047_ = lazy (Model.get (Lazy.force new_state__024_) 0) in
            (try
               (match v_3 with
                | None ->
                    if
                      (Lazy.force t_new__047_).contents =
                        Ortac_runtime.Gospelstdlib.Sequence.empty
                    then true
                    else false
                | Some a_1 ->
                    if
                      a_1 =
                        (Ortac_runtime.Gospelstdlib.Sequence.hd
                           (Lazy.force t_new__047_).contents)
                    then true
                    else false)
                 = true
             with
             | e ->
                 raise
                   (Ortac_runtime.Partial_function
                      (e,
                        {
                          Ortac_runtime.start =
                            {
                              pos_fname = "queue.mli";
                              pos_lnum = 32;
                              pos_bol = 1344;
                              pos_cnum = 1356
                            };
                          Ortac_runtime.stop =
                            {
                              pos_fname = "queue.mli";
                              pos_lnum = 34;
                              pos_bol = 1415;
                              pos_cnum = 1461
                            }
                        })))
          then None
          else
            Some
              (Ortac_runtime.report "Queue" "create ()"
                 (Either.right (Res (Ortac_runtime.dummy, ()))) "peek_opt"
                 [("match v with\n        | None -> t.contents = Sequence.empty\n        | Some a -> a = Sequence.hd t.contents",
                    {
                      Ortac_runtime.start =
                        {
                          pos_fname = "queue.mli";
                          pos_lnum = 32;
                          pos_bol = 1344;
                          pos_cnum = 1356
                        };
                      Ortac_runtime.stop =
                        {
                          pos_fname = "queue.mli";
                          pos_lnum = 34;
                          pos_bol = 1415;
                          pos_cnum = 1461
                        }
                    })])
      | (Clear, Res ((Unit, _), _)) -> None
      | (Is_empty, Res ((Bool, _), b)) ->
          if
            let t_old__053_ = Model.get state__022_ 0
            and t_new__054_ = lazy (Model.get (Lazy.force new_state__024_) 0) in
            (try
               b =
                 (match Ortac_runtime.Gospelstdlib.Sequence.length
                          (Lazy.force t_new__054_).contents
                  with
                  | __x__055_ when
                      (=) __x__055_
                        (Ortac_runtime.Gospelstdlib.integer_of_int 0)
                      -> true
                  | _ -> false)
             with
             | e ->
                 raise
                   (Ortac_runtime.Partial_function
                      (e,
                        {
                          Ortac_runtime.start =
                            {
                              pos_fname = "queue.mli";
                              pos_lnum = 43;
                              pos_bol = 1799;
                              pos_cnum = 1811
                            };
                          Ortac_runtime.stop =
                            {
                              pos_fname = "queue.mli";
                              pos_lnum = 45;
                              pos_bol = 1873;
                              pos_cnum = 1893
                            }
                        })))
          then None
          else
            Some
              (Ortac_runtime.report "Queue" "create ()"
                 (Either.right
                    (Res
                       (bool,
                         (let t_old__050_ = Model.get state__022_ 0
                          and t_new__051_ =
                            lazy (Model.get (Lazy.force new_state__024_) 0) in
                          try
                            match Ortac_runtime.Gospelstdlib.Sequence.length
                                    (Lazy.force t_new__051_).contents
                            with
                            | __x__052_ when
                                (=) __x__052_
                                  (Ortac_runtime.Gospelstdlib.integer_of_int
                                     0)
                                -> true
                            | _ -> false
                          with
                          | e ->
                              raise
                                (Ortac_runtime.Partial_function
                                   (e,
                                     {
                                       Ortac_runtime.start =
                                         {
                                           pos_fname = "queue.mli";
                                           pos_lnum = 43;
                                           pos_bol = 1799;
                                           pos_cnum = 1815
                                         };
                                       Ortac_runtime.stop =
                                         {
                                           pos_fname = "queue.mli";
                                           pos_lnum = 45;
                                           pos_bol = 1873;
                                           pos_cnum = 1893
                                         }
                                     })))))) "is_empty"
                 [("b = match Sequence.length t.contents with\n        | 0 -> true\n        | _ -> false",
                    {
                      Ortac_runtime.start =
                        {
                          pos_fname = "queue.mli";
                          pos_lnum = 43;
                          pos_bol = 1799;
                          pos_cnum = 1811
                        };
                      Ortac_runtime.stop =
                        {
                          pos_fname = "queue.mli";
                          pos_lnum = 45;
                          pos_bol = 1873;
                          pos_cnum = 1893
                        }
                    })])
      | (Transfer, Res ((Unit, _), _)) -> None
      | _ -> None
let _ =
  QCheck_base_runner.run_tests_main
    (let count = 1000 in
     [STMTests.agree_test ~count ~name:"Queue STM tests" check_init_state
        ortac_postcond])

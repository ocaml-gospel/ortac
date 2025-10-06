open Hashtbl

let init_sut = create ~random:false 16

type sut = (char, int) t

let arb_cmd state =
  let open QCheck in
  make ~print:show_cmd
    (let open Gen in
     let keys =
       let open Ortac_runtime.Gospelstdlib in
       List.of_seq @@ Sequence.map fst @@ (Model.get state 0).ModelElt.contents
     in
     let char =
       match keys with [] -> char | xs -> oneof [ char; oneofl xs ]
     in
     frequency
       [
         ( 1,
           pure (fun random -> fun size -> Create (random, size))
           <*> bool
           <*> small_signed_int );
         (1, pure Clear);
         (1, pure Reset);
         (1, pure Copy);
         (1, pure (fun a_1 -> fun b_1 -> Add (a_1, b_1)) <*> char <*> int);
         (1, pure (fun a_2 -> Find a_2) <*> char);
         (1, pure (fun a_3 -> Find_opt a_3) <*> char);
         (1, pure (fun a_4 -> Find_all a_4) <*> char);
         (1, pure (fun a_5 -> Mem a_5) <*> char);
         (1, pure (fun a_6 -> Remove a_6) <*> char);
         (1, pure (fun a_7 -> fun b_2 -> Replace (a_7, b_2)) <*> char <*> int);
         ( 1,
           pure (fun f -> Filter_map_inplace f)
           <*> (fun2 Observable.char Observable.int (QCheck.option QCheck.int))
                 .gen );
         (1, pure Length);
       ])

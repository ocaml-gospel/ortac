let init_sut = create ~random:false 16

type sut = (char, int) t

let gen_cmd state =
  let open QCheck in
  let open Gen in
  let keys =
    let open Ortac_runtime.Gospelstdlib in
    List.of_seq @@ Sequence.map fst @@ (Model.get state 0).ModelElt.contents
  in
  let char =
    match keys with [] -> char | xs -> oneof [ char; oneof_list xs ]
  in
  oneof
    [
      (fun random size -> Create (random, size)) <$> bool <*> nat_small;
      pure Clear;
      pure Reset;
      pure Copy;
      (fun k v -> Add (k, v)) <$> char <*> int;
      (fun k -> Find k) <$> char;
      (fun k -> Find_opt k) <$> char;
      (fun k -> Find_all k) <$> char;
      (fun k -> Mem k) <$> char;
      (fun k -> Remove k) <$> char;
      (fun k v -> Replace (k, v)) <$> char <*> int;
      (fun f -> Filter_map_inplace f)
      <$> (fun2 Observable.char Observable.int (QCheck.option QCheck.int)).gen;
      pure Length;
    ]

let gen_cmd state =
  (* We will use [fun2] which is in [QCheck] module *)
  let open QCheck in
  let open Gen in
  (* We inspect the [state] to compute the list of keys. This will be used by
     functions that take only one SUT, so we look at the first model in the
     model container. *)
  let keys =
    (* We use the [Ortac_runtime.Gospelstdlib.Sequence] module to manipulate
       the [contents] field of the model of one hash table. *)
    let open Ortac_runtime.Gospelstdlib in
    (* The [Model] and [ModelElt] are declared in the generated code, hence are
       directly available where this code will be injected. *)
    List.of_seq @@ Sequence.map fst @@ (Model.get state 0).ModelElt.contents
  in
  (* We use the keys to implement a new [key] generator and make sure we have a
     chance to generate a key that is actualy mapped to a value. *)
  let key =
    match keys with [] -> char | xs -> oneof [ char; oneof_list xs ]
  in
  (* The following is classic QCheck generator. *)
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

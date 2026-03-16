type sut = int t

let init_sut = make 42

module Gen = struct
  (* to avoid overflow *)
  let int = nat_small
end

module Weights_dom1 = struct
  let get = 2
end

let gen_cmd_dom0 _ =
  let open QCheck.Gen in
  oneof_weighted
    [
      (1, pure Get);
      (1, pure (fun v_1 -> Set v_1) <*> int);
      (42, pure (fun v_2 -> Exchange v_2) <*> int);
      ( 1,
        pure (fun seen -> fun v_3 -> Compare_and_set (seen, v_3))
        <*> int
        <*> int );
      (1, pure (fun n -> Fetch_and_add n) <*> int);
      (1, pure Incr);
      (1, pure Decr);
    ]

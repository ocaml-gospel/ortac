type sut = int t

let init_sut = create ()

module Gen = struct
  let int = nat
  let list = small_list
end

(* A command generator for the non-consumer domain *)
let arb_cmd_dom1 _ =
  let open QCheck in
    make ~print:show_cmd
      (let open Gen in
          frequency
            [(0, ((pure (fun () -> Create ())) <*> unit));
            (0,
              ((pure (fun xs -> Of_list xs)) <*> (list small_signed_int)));
            (1, ((pure (fun a_2 -> Push a_2)) <*> int));
            (1, ((pure (fun xs_1 -> Push_all xs_1)) <*> (list int)));
            (0, (pure Is_empty));
            (0, (pure Close));
            (0, (pure Pop_exn));
            (0, (pure Pop_opt));
            (0, (pure Drop_exn));
            (0, (pure Peek_exn));
            (0, (pure Peek_opt));
            (0, ((pure (fun a_3 -> Push_head a_3)) <*> int))])

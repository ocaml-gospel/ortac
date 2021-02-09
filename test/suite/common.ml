let check_raises_gospel msg f =
  Alcotest.(check pass) msg () ();
  try
    f ();
    Alcotest.failf "expecting Gospel failure, got nothing"
  with
  | Gospel_runtime.Error _ -> ()
  | e ->
      Alcotest.failf "expecting Gospel failure, got %s" (Printexc.to_string e)

let check_success msg f =
  Alcotest.(check pass) msg () ();
  f ()

let check_raises = Alcotest.check_raises

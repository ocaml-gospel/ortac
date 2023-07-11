let check_raises_ortac msg f =
  Alcotest.(check pass) msg () ();
  try
    f ();
    Alcotest.failf "expecting Ortac failure, got nothing"
  with
  | Ortac_runtime.Error _ -> ()
  | e -> Alcotest.failf "expecting Ortac failure, got %s" (Printexc.to_string e)

let check_success msg f =
  Alcotest.(check pass) msg () ();
  f ()

let check_raises = Alcotest.check_raises

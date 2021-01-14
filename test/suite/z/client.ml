open Lib_rtac

let () = Fmt.(set_style_renderer stderr `Ansi_tty)

let test_success f =
  try
    f ();
    Fmt.epr "Success test OK@."
  with
  | Gospel_runtime.Error e ->
      Fmt.epr "%a" Gospel_runtime.report e;
      exit 1
  | e ->
      Fmt.epr "UNEXPECTED ERROR.@\n";
      (* raise e *)
      ()

let test_failure f =
  try
    f ();
    Fmt.epr "Should fail.@.";
    exit 1
  with
  | Gospel_runtime.Error e -> Fmt.epr "Failure test OK@."
  | e ->
      Fmt.epr "UNEXPECTED ERROR.@\n";
      (* raise e *)
      ()

let () = test_success (fun () -> ignore (test_forall (-2) (-3)))

let () = test_success (fun () -> ignore (test_forall (-2) (-2)))

let () = test_failure (fun () -> ignore (test_forall (-2) (-1)))

let () = test_success (fun () -> ignore (test_forall 2 3))

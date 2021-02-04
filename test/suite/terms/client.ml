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

let () = test_success (fun () -> ignore (lazy_bool 42))

let () = test_failure (fun () -> ignore (not_lazy_bool1 42))

let () = test_failure (fun () -> ignore (not_lazy_bool2 42))

let () = test_success (fun () -> ignore (scope1 42))

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

let () = test_failure (fun () -> ignore (create (-10) 0))

let () =
  test_failure (fun () ->
      let a = create 10 0 in
      ignore (bad_get a 9))

let () =
  test_failure (fun () ->
      let a = create 10 0 in
      set a 8 1;
      ignore (bad_get a 8))

let () = test_failure (fun () -> ignore (bad_create 10 1729))

let test1 () =
  let arr = create 10 0 in
  let o = get arr 2 in
  assert (o = 0);
  set arr 3 42;
  let v = get arr 3 in
  assert (v = 42);
  fill arr 3 7 1729;
  assert (get arr 3 = 1729)

let () = test_success test1

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
  | e -> Fmt.epr "UNEXPECTED ERROR.@\n"

let test_failure f =
  try
    f ();
    Fmt.epr "Should fail.@.";
    exit 1
  with
  | Gospel_runtime.Error e -> Fmt.epr "Failure test OK@."
  | e -> Fmt.epr "UNEXPECTED ERROR.@\n"

let test_otherfailure f exn =
  try
    f ();
    Fmt.epr "Should fail.@.";
    exit 1
  with
  | Gospel_runtime.Error e ->
      Fmt.epr "UNEXPECTED ERROR.@\n";
      exit 1
  | e when e <> exn -> Fmt.epr "UNEXPECTED ERROR.@\n"
  | exn -> Fmt.epr "Failure test OK@."

let () =
  test_failure (fun () -> bad_raise_notfound 0 |> ignore);
  test_failure (fun () -> undeclared_raise_notfound 0 |> ignore);
  test_failure (fun () -> raise_invalidarg "not the right string" |> ignore)

let () =
  test_otherfailure (fun () -> raise_notfound 0 |> ignore) Not_found;
  test_otherfailure (fun () -> raise_oom 0 |> ignore) Out_of_memory;
  test_otherfailure (fun () -> raise_stackoverflow 0 |> ignore) Stack_overflow;
  test_otherfailure
    (fun () -> raise_invalidarg "invalid" |> ignore)
    (Invalid_argument "invalid")

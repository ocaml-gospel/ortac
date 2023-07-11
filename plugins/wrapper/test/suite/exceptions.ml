open Exceptions__Lib_rtac
open Common

let bad_raise_notfound () =
  check_raises_ortac "bad_raise_notfound" (fun () ->
      bad_raise_notfound 0 |> ignore)

let undeclared_raise_notfound () =
  check_raises_ortac "bad_raise_notfound" (fun () ->
      undeclared_raise_notfound 0 |> ignore)

let raise_invalidarg () =
  check_raises_ortac "raise_invalidarg with bad string" (fun () ->
      raise_invalidarg "not the right string" |> ignore);
  check_raises "raise_invalidarg with correct string"
    (Invalid_argument "invalid") (fun () ->
      raise_invalidarg "invalid" |> ignore)

let raise_notfound () =
  check_raises "raise_notfound" Not_found (fun () -> raise_notfound 0 |> ignore)

let raise_std () =
  check_raises "raise_oom" Out_of_memory (fun () -> raise_oom 0 |> ignore);
  check_raises "raise_stackoverflow" Stack_overflow (fun () ->
      raise_stackoverflow 0 |> ignore)

let valid_checks () =
  check_success "check true" (fun () -> check true |> ignore);
  check_raises "check false" (Invalid_argument "invalid") (fun () ->
      check false |> ignore);
  check_raises "double_check 11" (Invalid_argument ">= 10") (fun () ->
      double_check 11 |> ignore);
  check_raises "double_check (-1)" (Invalid_argument "<= 0") (fun () ->
      double_check (-1) |> ignore);
  check_success "double_check 1" (fun () -> double_check 1 |> ignore)

let bad_checks () =
  check_raises_ortac "bad_check_modifies (ref false)" (fun () ->
      bad_check_modifies (ref false) |> ignore);
  check_success "bad_check_modifies (ref true)" (fun () ->
      bad_check_modifies (ref true) |> ignore);
  check_raises_ortac "bad_check true" (fun () -> bad_check true |> ignore);
  check_raises "bad_check false" (Invalid_argument "invalid") (fun () ->
      bad_check false |> ignore);
  check_success "bad_check2 true" (fun () -> bad_check2 true |> ignore);
  check_raises_ortac "bad_check2 false" (fun () -> bad_check2 false |> ignore)

let suite =
  ( "Exceptions",
    [
      ("bad_raise_notfound", `Quick, bad_raise_notfound);
      ("undeclared_raise_notfound", `Quick, undeclared_raise_notfound);
      ("raise_notfound", `Quick, raise_notfound);
      ("allowed exceptions", `Quick, raise_std);
      ("raise_invalidarg", `Quick, raise_invalidarg);
      ("bad_raise_notfound", `Quick, bad_raise_notfound);
      ("valid_checks", `Quick, valid_checks);
      ("bad_checks", `Quick, bad_checks);
    ] )

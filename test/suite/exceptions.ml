open Exceptions__Lib_rtac
open Common

let bad_raise_notfound () =
  check_raises_gospel "bad_raise_notfound" (fun () ->
      bad_raise_notfound 0 |> ignore)

let undeclared_raise_notfound () =
  check_raises_gospel "bad_raise_notfound" (fun () ->
      undeclared_raise_notfound 0 |> ignore)

let raise_invalidarg () =
  check_raises_gospel "raise_invalidarg with bad string" (fun () ->
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

let suite =
  ( "Exceptions",
    [
      ("bad_raise_notfound", `Quick, bad_raise_notfound);
      ("undeclared_raise_notfound", `Quick, undeclared_raise_notfound);
      ("raise_notfound", `Quick, raise_notfound);
      ("allowed exceptions", `Quick, raise_std);
      ("raise_invalidarg", `Quick, raise_invalidarg);
      ("bad_raise_notfound", `Quick, bad_raise_notfound);
    ] )

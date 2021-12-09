open Ltypes__Lib_rtac
open Common

let get_x_ok () =
  let t = { x = 10; y = false } in
  check_success "get_x is ok" (fun () -> get_x t |> ignore)

let get_x_wrong_invariant () =
  let t = { x = 10; y = true } in
  check_raises_ortac "get_x with wrong invariant" (fun () -> get_x t |> ignore)

let in_list () =
  let l = [ { x = 10; y = true } ] in
  check_raises_ortac "in_list" (fun () -> in_list l |> ignore)

let suite =
  ( "Types",
    [
      ("simple field access", `Quick, get_x_ok);
      ("invariant check", `Quick, get_x_wrong_invariant);
      ("invariant in list", `Quick, in_list);
    ] )

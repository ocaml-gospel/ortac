open Ltypes__Lib_rtac
open Common

let get_x () =
  let t = check_success "v" (fun () -> v 10 false) in
  check_success "get_x" (fun () -> get_x t |> ignore)

let invariant_breat () =
  check_raises_ortac "invariant_break" (fun () -> v 10 true |> ignore)

let suite =
  ( "Types",
    [
      ("simple field access", `Quick, get_x);
      ("invalid v function", `Quick, invariant_breat);
    ] )

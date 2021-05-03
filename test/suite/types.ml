open Ltypes__Lib_rtac
open Common

let get_x () =
  let t = { x = 10; y = false } in
  check_success "get_x" (fun () -> get_x t |> ignore)

let suite = ("Types", [ ("simple field access", `Quick, get_x) ])

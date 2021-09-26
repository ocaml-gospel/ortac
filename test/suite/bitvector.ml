open Bitvector__Lib_rtac
open Common

let create () =
  check_success "good create" (fun () -> create 10 |> ignore);
  check_raises_ortac "bad create" (fun () -> create 70 |> ignore)

let suite = ("Bitvector", [ ("create", `Quick, create) ])

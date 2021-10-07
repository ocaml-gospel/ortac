open Arrays__Lib_rtac
open Common

let bad_get_oor () =
  let arr =
    check_success "create with right arguments" (fun () -> create 10 0)
  in
  check_raises_ortac "bad_get is out of range" (fun () ->
      bad_get arr 9 |> ignore)

let bad_get_wrong_value () =
  let arr =
    check_success "create with right arguments" (fun () -> create 10 0)
  in
  check_success "set with right arguments" (fun () -> set arr 8 1);
  check_raises_ortac "bad_get doesn't return the right value" (fun () ->
      bad_get arr 8 |> ignore)

let bad_create () =
  check_raises_ortac "bad_create fills with wrong values" (fun () ->
      bad_create 10 1729 |> ignore)

let normal () =
  check_success "correct implementation" (fun () ->
      let arr = create 10 0 in
      let _ = get arr 2 in
      set arr 3 42;
      let _ = get arr 3 in
      fill arr 3 7 1729)

let map () =
  check_success "map correct" (fun () ->
      let a = create 10 0 in
      map (fun x -> x + 1) a |> ignore)

let bad_map_length () =
  check_raises_ortac "bad_map_length returns an empty array" (fun () ->
      let a = create 10 0 in
      bad_map_length (fun x -> x + 1) a |> ignore)

let bad_map_fun () =
  check_raises_ortac "bad_map_fun apply another function" (fun () ->
      let a = create 10 0 in
      bad_map_fun (fun x -> x + 1) a |> ignore)

let sort_success () =
  check_success "sort array of int" (fun () -> sort [| 314; 42; 73; 57; 421 |])

let copy_sort_success () =
  check_success "copy_sort array of int" (fun () ->
      copy_sort [| 314; 42; 73; 57; 421 |] |> ignore)

let bad_sort_fails () =
  check_raises_ortac "bad_sort returns always the same array" (fun () ->
      let arr = create 10 0 in
      bad_sort arr |> ignore)

let constant_sort_fails () =
  check_raises_ortac
    "constant_sort returns arrays with rigth length but wrong values" (fun () ->
      constant_sort [| 314; 42; 73; 57; 421 |] |> ignore)

let create () =
  check_raises_ortac "create with negative length" (fun () ->
      create (-10) 0 |> ignore)

let suite =
  ( "Arrays",
    [
      ("get out of range", `Quick, bad_get_oor);
      ("get wrong value", `Quick, bad_get_wrong_value);
      ("create wrong value", `Quick, bad_create);
      ("correct implementations", `Quick, normal);
      ("map correct", `Quick, map);
      ("map wrong length", `Quick, bad_map_length);
      ("map wrong function", `Quick, bad_map_fun);
      ("create with wrong arguments", `Quick, create);
      ("sort is correct", `Quick, sort_success);
      ("copy sort is correct", `Quick, copy_sort_success);
      ("bad sort is wrong", `Quick, bad_sort_fails);
      ("constant sort is wrong", `Quick, constant_sort_fails);
    ] )

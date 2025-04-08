module To_test = Wrapper

(* create *)
let test_create () =
  let s = To_test.create 5 in
  Alcotest.(check int)  "size should be 5" 5 s.size;
  Alcotest.(check int)  "initial mask" 0 s.mask;
  Alcotest.(check bool) "3 should not be in set" false (To_test.mem 3 s)

let test_neg_create () =
  try
    let _ = To_test.create (-1) in
    Alcotest.fail "Negativ size"
  with Ortac_runtime.Error _ -> ()

(* add *)
let test_add () =
  let s = To_test.create 5 in
  To_test.add 2 s;
  Alcotest.(check bool) "2 should be in set" true (To_test.mem 2 s);
  Alcotest.(check bool) "3 should not be in set" false (To_test.mem 3 s)

let test_neg_add () =
  let s = To_test.create 5 in
  try
    let _ = To_test.add (-1) s in
    Alcotest.fail "Negativ argument"
  with Ortac_runtime.Error _ -> ()

(* mem *)
let test_neg_mem () =
  let s = To_test.create 5 in
  try
    let _ = To_test.mem (-1) s in
    Alcotest.fail "Negativ argument"
  with Ortac_runtime.Error _ -> ()

let () =
  let open Alcotest in
  run "Wrapped lib" [
    "create", [
      test_case "create is correct."              `Quick test_create;
      test_case "create fails with negativ size." `Quick test_neg_create;
    ];
    "add", [
      test_case "add is correct."             `Quick test_add;
      test_case "add fails with negativ arg." `Quick test_neg_add;
    ];
    "mem", [
      test_case "mem fails with negativ arg"  `Quick test_neg_mem;
    ];
  ]
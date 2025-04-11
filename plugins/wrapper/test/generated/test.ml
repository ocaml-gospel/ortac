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


let vio_inv () =
(* Expected without `try ... with`
Runtime error in function `bad_create_int'
  - the invariant
      `t.value >= 0'
    was violated in the post-state.
*)
  try
    let _ = To_test.bad_create_int (-1) in
    Alcotest.fail "Arg should be positiv."
  with Ortac_runtime.Error _ -> ()

let vio_pre () =
(* Expected without `try ... with`
Runtime error in function `create_int'
  - the pre-condition
      `n >= 0'
    was violated.
*)
  try
    let _ = To_test.create_int (-1) in
    Alcotest.fail "Arg should be > 0"
  with Ortac_runtime.Error _ -> ()

let good_incr () =
  let n = To_test.create_int 0 in
  let r = To_test.increment_int n in
  Alcotest.(check int) "incr 0 should be 1" 1 r;
  ()

let incr_maxint () =
  let n = To_test.create_int max_int in
  try
    let _ = To_test.bad_increment_int n in
    Alcotest.fail "Arg should be < int_max"
  with To_test.Int_overflow -> ()

let inv_arg () =
  let n = To_test.create_int 1 in
  try
    let _ = To_test.bad2_increment_int n in
    Alcotest.fail "Arg should not be 1 (checks clause)"
  with Invalid_argument _ -> ()

let good_mut_incr () =
  let n = To_test.create_int 0 in
  To_test.mut_incr n;
  Alcotest.(check int) "incr 0 should be 1" 1 n.value;
  ()

let bad_mut_incr () =
  let n = To_test.create_int 0 in
  try
    let _ = To_test.bad_mut_incr n in
    Alcotest.fail "function should incr"
  with Ortac_runtime.Error _ -> ()


let () =
  let open Alcotest in
  run "Wrapped lib" [
    "lib", [
      test_case "create is correct."              `Quick test_create;
      test_case "create fails with negativ size." `Quick test_neg_create;
      test_case "add is correct."                 `Quick test_add;
      test_case "add fails with negativ arg."     `Quick test_neg_add;
      test_case "mem fails with negativ arg"      `Quick test_neg_mem;
    ];
    "int", [
      test_case "invariant violated hit"                `Quick vio_inv;
      test_case "good incrementation"                   `Quick good_incr;
      test_case "precondition violated hit"             `Quick vio_pre;
      test_case "Int_overflow raised"                   `Quick incr_maxint;
      test_case "checks no hold, Invalid_argument hit"  `Quick inv_arg;
      test_case "good mutable incr"                     `Quick good_mut_incr;
      test_case "postcondition hit"                     `Quick bad_mut_incr;
    ];
  ]

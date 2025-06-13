module To_test = Wrapper
module To_test2 = Wrapper_behaviour
module To_test3 = Wrapper_model
module Pascal = Pascal_wrapped

let test_create () =
  let s = To_test.create 5 in
  Alcotest.(check int) "size should be 5" 5 s.size;
  Alcotest.(check int) "initial mask" 0 s.mask;
  Alcotest.(check bool) "3 should not be in set" false (To_test.mem 3 s)

let test_neg_create () =
  try
    let _ = To_test.create (-1) in
    Alcotest.fail "Negativ size"
  with Ortac_runtime.Error _ -> ()

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

let test_neg_mem () =
  let s = To_test.create 5 in
  try
    let _ = To_test.mem (-1) s in
    Alcotest.fail "Negativ argument"
  with Ortac_runtime.Error _ -> ()

let vio_inv () =
  (* -- Expected message without error handling --
Runtime error in function `bad_create_int'
  - the invariant
      `t.value >= 0'
    was violated in the post-state.
*)
  try
    let _ = To_test2.bad_create_int (-1) in
    Alcotest.fail "Arg should be positiv."
  with Ortac_runtime.Error _ -> ()

let vio_pre () =
  (* -- Expected message without error handling --
Runtime error in function `create_int'
  - the pre-condition
      `n >= 0'
    was violated.
*)
  try
    let _ = To_test2.create_int (-1) in
    Alcotest.fail "Arg should be > 0"
  with Ortac_runtime.Error _ -> ()

let good_incr () =
  let n = To_test2.create_int 0 in
  let r = To_test2.increment_int n in
  Alcotest.(check int) "incr 0 should be 1" 1 r;
  ()

let incr_maxint () =
  let n = To_test2.create_int max_int in
  try
    let _ = To_test2.bad_increment_int n in
    Alcotest.fail "Increment on int_max should raise Int_overflow"
  with To_test2.Int_overflow -> ()

let inv_arg () =
  (* -- Expected message without error handling --
[invalid] According to checks clause, x.value should not be 1.
          Raised at Stdlib.invalid_arg in file
*)
  let n = To_test2.create_int 1 in
  try
    let _ = To_test2.bad2_increment_int n in
    Alcotest.fail "Arg should not be 1 (checks clause)"
  with Invalid_argument _ -> ()

let post_cond () =
  (* -- Expected message without error handling --
Runtime error in function `bad2_increment_int'
  - the post-condition
      `r = x.value + 1'
    was violated.
*)
  let n = To_test2.create_int 0 in
  try
    let _ = To_test2.bad2_increment_int n in
    Alcotest.fail "n should be incremented"
  with Ortac_runtime.Error _ -> ()

let add_model () =
  let s = To_test3.create 4 in
  To_test3.add s 1;
  To_test3.add s 2;
  To_test3.add s 3

let pascal () =
  let p = Pascal.init () in
  Pascal.next p;
  Pascal.next p;
  Pascal.next p;
  ()

let () =
  let open Alcotest in
  run "Wrapped lib"
    [
      ( "lib",
        [
          test_case "create is correct." `Quick test_create;
          test_case "create fails with negativ size." `Quick test_neg_create;
          test_case "add is correct." `Quick test_add;
          test_case "add fails with negativ arg." `Quick test_neg_add;
          test_case "mem fails with negativ arg" `Quick test_neg_mem;
        ] );
      ( "behaviour tests",
        [
          test_case "invariant violated hit" `Quick vio_inv;
          test_case "good incrementation" `Quick good_incr;
          test_case "precondition violated hit" `Quick vio_pre;
          test_case "Int_overflow raised" `Quick incr_maxint;
          test_case "checks no hold, Invalid_argument hit" `Quick inv_arg;
          test_case "postcondition hit" `Quick post_cond;
        ] );
      ("model tests", [ test_case "add model" `Quick add_model ]);
      ("pascal tests", [ test_case "three nexts" `Quick pascal ]);
    ]

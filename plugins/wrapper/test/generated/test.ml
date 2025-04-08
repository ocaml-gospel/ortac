module To_test = struct
  let create = Lib.create
  let add = Lib.add
  let mem = Lib.mem
end

let test_create () =
  let s = To_test.create 5 in
  Alcotest.(check int) "size should be 5" 5 s.size;
  Alcotest.(check int) "initial mask" 0 s.mask

let test_add_mem () =
  let s = To_test.create 8 in
  To_test.add 2 s;
  Alcotest.(check bool) "2 should be in set" true (To_test.mem 2 s);
  Alcotest.(check bool) "3 should not be in set" false (To_test.mem 3 s)

let () =
  let open Alcotest in
  run "Expected" [
    "lib.set", [
      test_case "Create set"     `Quick test_create;
      test_case "Add and mem"    `Quick test_add_mem;
    ];
  ]
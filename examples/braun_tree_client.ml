open Braun_tree_wrapped

let () =
  let b = of_list [ 1; 2; 3; 4 ] in
  let b = cons 0 b in
  let b = snoc 5 b in
  let b = tail b in
  let _b = liat b in
  ()

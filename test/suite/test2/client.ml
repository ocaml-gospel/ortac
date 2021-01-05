open Lib_rtac

let () =
  let arr = create 10 0 in
  let o = get arr 2 in
  assert (o = 0)

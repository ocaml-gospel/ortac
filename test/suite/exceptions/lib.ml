let raise_oom _ = raise Out_of_memory
let raise_stackoverflow _ = raise Stack_overflow
let raise_notfound _ = raise Not_found
let bad_raise_notfound _ = raise Not_found
let undeclared_raise_notfound _ = raise Not_found
let raise_invalidarg _ = invalid_arg "invalid"
let check b = if not b then invalid_arg "invalid" else b

let bad_check_modifies b =
  b := true;
  !b

let double_check x =
  if x <= 0 then raise (Invalid_argument "<= 0");
  if x >= 10 then raise (Invalid_argument ">= 10");
  x

let bad_check _ = invalid_arg "invalid"
let bad_check2 b = b

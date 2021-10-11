let raise_oom _ = raise Out_of_memory
let raise_stackoverflow _ = raise Stack_overflow
let raise_notfound _ = raise Not_found
let bad_raise_notfound _ = raise Not_found
let undeclared_raise_notfound _ = raise Not_found
let raise_invalidarg _ = invalid_arg "invalid"

type t = { value : int }

exception Int_overflow

let create_int n =
  if n < 0 then failwith "Precondition failed: n must be non-negative"
  else { value = n }

let bad_create_int n = { value = n }

let increment_int x =
  if x.value = max_int then raise Int_overflow else x.value + 1

let bad_increment_int x = increment_int x

let bad2_increment_int x =
  match x.value with
  | 1 -> invalid_arg "According to checks clause, x.value should not be 1."
  | n when n = max_int -> raise Int_overflow
  | _ -> x.value

type 'a t
(*@ mutable model contents : 'a list *)

val make : int -> 'a -> 'a t
(*@ t = make i a
    checks i >= 0
    ensures t.contents = List.init i (fun j -> a) *)

val sut_in_type : 'a t list -> int
(*@ r = sut_in_type ts
    ensures r = List.length ts *)

val sut_in_tuple : 'a t * int -> bool
(*@ r = sut_in_tuple a
    ensures r = true *)

val sut_in_nested_type : 'a t list list -> bool
(*@ r = sut_in_nested_type ts
    ensures r = true *)

val sut_nested_in_tuple : 'a t list * int -> bool
(*@ r = sut_nested_in_tuple a
    ensures r = true *)

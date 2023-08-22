val create : int -> 'a -> 'a array
(*@ arr = create n v
    requires n >= 0
    ensures Array.length arr = n
    ensures Array.for_all (fun x -> x = v) arr
    ensures forall i. 0 <= i < n -> arr.(i) = v *)

val bad_create : int -> int -> int array
(*@ arr = bad_create n v
    ensures forall i. 0 <= i < n -> arr.(i) = v *)

val get : 'a array -> int -> 'a
(*@ o = get arr i
    requires 0 <= i < Array.length arr
    ensures o = arr.(i) *)

(*@ axiom a :
      let arr = Array.make 10 0 in
      forall i. 0 <= i < 10 -> arr.(i) = 0 *)

val bad_get : 'a array -> int -> 'a
(*@ o = bad_get arr i
    requires 0 <= i < Array.length arr
    ensures o = arr.(i) *)

val set : 'a array -> int -> 'a -> unit
(*@ set arr i v
    modifies arr
    requires 0 <= i < Array.length arr
    ensures  arr.(i) = v
    (*
     ensures  forall j. 0 <= j < Array.length arr -> j <> i ->
             arr.(j) = old arr.(j) *)
*)

val fill : 'a array -> int -> int -> 'a -> unit
(*@ fill arr ofs len v
    modifies arr
    requires 0 <= ofs <= ofs + len <= Array.length arr
    ensures  forall j. ofs <= j < ofs + len -> arr.(j) = v *)

val length : 'a array -> int
(*@ i = length a
    pure
    ensures i = Array.length a *)

val map : ('a -> 'b) -> 'a array -> 'b array
(*@ arr = map f a
    ensures length arr = length a
    ensures forall i. 0 <= i < length a -> arr.(i) = f a.(i) *)

val bad_map_length : ('a -> 'b) -> 'a array -> 'b array
(*@ arr = bad_map_length f a
    ensures length arr = length a
    ensures forall i. 0 <= i < length a -> arr.(i) = f a.(i) *)

val bad_map_fun : (int -> int) -> int array -> int array
(*@ arr = bad_map_fun f a
    ensures length arr = length a
    ensures forall i. 0 <= i < length a -> arr.(i) = f a.(i) *)

val sort : int array -> unit
(*@ sort a
    modifies a
    ensures forall i. 0 <= i < Array.length a
            -> forall j. i < j < Array.length a
            -> a.(i) <= a.(j) *)

val copy_sort : int array -> int array
(*@ r = copy_sort a
    ensures Array.length r = Array.length a
    ensures forall i. 0 <= i < Array.length r
            -> forall j. i < j < Array.length r
            -> r.(i) <= r.(j)
    ensures forall i. 0 <= i < Array.length r
            -> exists j. 0 <= j < Array.length a /\ r.(i) = a.(j)
    ensures forall i. 0 <= i < Array.length a
            -> exists j. 0 <= j < Array.length r /\ a.(i) = r.(j) *)

val bad_sort : int array -> int array
(*@ r = bad_sort a
    ensures forall i. 0 <= i < Array.length r
            -> forall j. i < j < Array.length r
            -> r.(i) <= r.(j) *)

val constant_sort : int array -> int array
(*@ r = constant_sort a
    ensures Array.length r = Array.length a
    ensures forall i. 0 <= i < Array.length r
            -> forall j. i < j < Array.length r
            -> r.(i) <= r.(j)
    ensures forall i. 0 <= i < Array.length r
            -> exists j. 0 <= j < Array.length a /\ r.(i) = a.(j)
    ensures forall i. 0 <= i < Array.length a
            -> exists j. 0 <= j < Array.length r /\ a.(i) = r.(j) *)

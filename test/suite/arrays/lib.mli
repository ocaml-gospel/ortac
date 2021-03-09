val create : int -> 'a -> 'a array
(*@ arr = create n v
    requires n >= 0
    ensures length arr = n
    ensures forall i. 0 <= i < n -> arr[i] = v *)

val bad_create : int -> int -> int array
(*@ arr = bad_create n v
    ensures forall i. 0 <= i < n -> arr[i] = v *)

val get : 'a array -> int -> 'a
(*@ o = get arr i
    requires 0 <= i < length arr
    ensures o = arr[i] *)

val bad_get : 'a array -> int -> 'a
(*@ o = bad_get arr i
    requires 0 <= i < length arr
    ensures o = arr[i] *)

val set : 'a array -> int -> 'a -> unit
(*@ set arr i v
    requires 0 <= i < length arr
    ensures  arr[i] = v
    (*
     ensures  forall j. 0 <= j < length arr -> j <> i ->
             arr[j] = old arr[j] *)
    *)

val fill : 'a array -> int -> int -> 'a -> unit
(*@ fill arr ofs len v
    requires 0 <= ofs <= ofs + len <= length arr
    ensures  forall j. ofs <= j < ofs + len -> arr[j] = v *)

val sort : int array -> unit
(*@ sort a
    ensures forall i. 0 <= i < length a
            -> forall j. i < j < length a
            -> a[i] <= a[j] *)

val copy_sort : int array -> int array
(*@ r = copy_sort a
    ensures length r = length a
    ensures forall i. 0 <= i < length r
            -> forall j. i < j < length r
            -> r[i] <= r[j] *)

val bad_sort : int array -> int array
(*@ r = bad_sort a
    ensures forall i. 0 <= i < length r
            -> forall j. i < j < length r
            -> r[i] <= r[j] *)


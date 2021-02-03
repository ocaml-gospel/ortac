val create : int -> 'a -> 'a array
  [@@gospel
    {| arr = create n v
    requires n >= 0
    ensures length arr = n
    ensures forall i. 0 <= i < n -> arr[i] = v |}]

val bad_create : int -> int -> int array
  [@@gospel
    {| arr = bad_create n v
    ensures forall i. 0 <= i < n -> arr[i] = v |}]

val get : 'a array -> int -> 'a
  [@@gospel
    {| o = get arr i
    requires 0 <= i < length arr
    ensures o = arr[i] |}]

val bad_get : 'a array -> int -> 'a
  [@@gospel
    {| o = bad_get arr i
    requires 0 <= i < length arr
    ensures o = arr[i] |}]

val set : 'a array -> int -> 'a -> unit
  [@@gospel
    {| set arr i v
    requires 0 <= i < length arr
    ensures  arr[i] = v
    (*
     ensures  forall j. 0 <= j < length arr -> j <> i ->
             arr[j] = old arr[j] *)
    |}]

val fill : 'a array -> int -> int -> 'a -> unit
  [@@gospel
    {| fill arr ofs len v
       requires 0 <= ofs <= ofs + len <= length arr
       ensures  forall j. ofs <= j < ofs + len -> arr[j] = v |}]

val create : int -> 'a -> 'a array
  [@@gospel
    {| arr = create n v
    ensures length arr = n
    ensures forall i. 0 <= i < n -> arr[i] = v |}]

val get : 'a array -> int -> 'a
  [@@gospel {| o = get arr i
    ensures o = arr[i] |}]

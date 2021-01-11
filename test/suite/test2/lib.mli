val create : int -> 'a -> 'a array
  [@@gospel
    {| arr = create n v
    requires n >= 0
    ensures length arr = n
    ensures forall i. 0 <= i < n -> arr[i] = v |}]

val get : 'a array -> int -> 'a
  [@@gospel
    {| o = get arr i
    requires 0 <= i < length arr
    ensures o = arr[i] |}]

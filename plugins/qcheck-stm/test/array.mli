type 'a t
(*@ model size : integer
    mutable model contents : 'a sequence *)

val length : 'a t -> int
(*@ i = length t
    ensures i = t.size *)

val get : 'a t -> int -> 'a
(*@ a = get t i
    checks 0 <= i < t.size
    ensures a = t.contents[i] *)

val set : 'a t -> int -> 'a -> unit
(*@ set t i a
    checks 0 <= i < t.size
    modifies t.contents
    ensures t.contents = Sequence.set (old t.contents) i a *)

val make : int -> 'a -> 'a t
(*@ t = make i a
    checks i >= 0
    ensures t.size = i
    ensures t.contents = Sequence.init i (fun j -> a) *)

val append : 'a t -> 'a t -> 'a t
(*@ t = append a b
    ensures t.size = a.size + b.size
    ensures t.contents = a.contents ++ b.contents *)

val sub : 'a t -> int -> int -> 'a t
(*@ r = sub t i n
    checks 0 <= i <= Sequence.length t.contents
    checks i <= i + n <= Sequence.length t.contents
    ensures r.size = n
    ensures r.contents = if n = 0 then Sequence.empty else t.contents[i..i+n-1] *)

val copy : 'a t -> 'a t
(*@ r = copy t
    ensures r.size = t.size
    ensures r.contents = t.contents *)

val fill : 'a t -> int -> int -> 'a -> unit
(*@ fill t pos len x
    checks 0 <= pos
    checks 0 <= len
    checks pos + len <= t.size
    modifies t.contents
    ensures t.contents = Sequence.init (Sequence.length (old t.contents)) (fun i -> if pos <= i < pos + len then x else (old t.contents)[i]) *)

val to_list : 'a t -> 'a list
(*@ l = to_list t
    ensures l = List.of_seq t.contents *)

val mem : 'a -> 'a t -> bool
(*@ b = mem a t
    ensures b = Sequence.mem t.contents a *)

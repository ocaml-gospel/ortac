type (!'a, !'b) t
(*@ mutable model contents : ('a * 'b) sequence *)

val create :
  ?random:(* thwart tools/sync_stdlib_docs *) bool -> int -> ('a, 'b) t
(*@ h = create ?random size
    ensures h.contents = Sequence.empty *)

val clear : ('a, 'b) t -> unit
(*@ clear h
    modifies h
    ensures h.contents = Sequence.empty *)

val reset : ('a, 'b) t -> unit
(*@ reset h
    modifies h
    ensures h.contents = Sequence.empty *)

val copy : ('a, 'b) t -> ('a, 'b) t
(*@ h2 = copy h1
    ensures h2.contents = h1.contents *)

val add : ('a, 'b) t -> 'a -> 'b -> unit
(*@ add h a b
    modifies h
    ensures h.contents = Sequence.cons (a, b) (old h.contents) *)

val find : ('a, 'b) t -> 'a -> 'b
(*@ b = find h a
    raises Not_found -> forall x. not (Sequence.mem h.contents (a, x))
    raises Not_found -> not (Sequence.mem (Sequence.map fst h.contents) a)
    ensures Sequence.mem h.contents (a, b) *)

val find_opt : ('a, 'b) t -> 'a -> 'b option
(*@ o = find_opt h a
    ensures match o with
      | None -> not (Sequence.mem (Sequence.map fst h.contents) a)
      | Some b -> Sequence.mem h.contents (a, b) *)

val find_all : ('a, 'b) t -> 'a -> 'b list
(*@ bs = find_all h a
    ensures bs = Sequence.filter_map (fun (x, y) -> if x = a then Some y else None) h.contents *)

val mem : ('a, 'b) t -> 'a -> bool
(*@ b = mem h a
    ensures b = Sequence.mem (Sequence.map fst h.contents) a *)

(*@ function rec remove_first (x: 'a) (xs : ('a * 'b) sequence) : ('a * 'b) sequence =
      if Sequence.empty = xs
      then xs
      else if fst (Sequence.hd xs) = x
           then Sequence.tl xs
           else Sequence.cons (Sequence.hd xs) (remove_first x (Sequence.tl xs)) *)

val remove : ('a, 'b) t -> 'a -> unit
(*@ remove h a
    modifies h
    ensures h.contents = remove_first a (old h.contents) *)

val replace : ('a, 'b) t -> 'a -> 'b -> unit
(*@ replace h a b
    modifies h
    ensures h.contents = Sequence.cons (a, b) (remove_first a (old h.contents)) *)

val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
val filter_map_inplace : ('a -> 'b -> 'b option) -> ('a, 'b) t -> unit
(*@ filter_map_inplace f h
    modifies h
    ensures h.contents = Sequence.filter_map
                            (fun (x,y) -> match f x y with
                                          | None -> None
                                          | Some b' -> Some (x, b'))
                            (old h.contents) *)

val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
val length : ('a, 'b) t -> int
(*@ i = length h
    ensures i = Sequence.length h.contents *)

val randomize : unit -> unit
val is_randomized : unit -> bool

val rebuild :
  ?random:(* thwart tools/sync_stdlib_docs *) bool -> ('a, 'b) t -> ('a, 'b) t

type statistics = {
  num_bindings : int;
      (** Number of bindings present in the table. Same value as returned by
          {!length}. *)
  num_buckets : int;  (** Number of buckets in the table. *)
  max_bucket_length : int;  (** Maximal number of bindings per bucket. *)
  bucket_histogram : int array;
      (** Histogram of bucket sizes. This array [histo] has length
          [max_bucket_length + 1]. The value of [histo.(i)] is the number of
          buckets whose size is [i]. *)
}

val stats : ('a, 'b) t -> statistics
val to_seq : ('a, 'b) t -> ('a * 'b) Seq.t
val to_seq_keys : ('a, _) t -> 'a Seq.t
val to_seq_values : (_, 'b) t -> 'b Seq.t
val add_seq : ('a, 'b) t -> ('a * 'b) Seq.t -> unit
val replace_seq : ('a, 'b) t -> ('a * 'b) Seq.t -> unit
val of_seq : ('a * 'b) Seq.t -> ('a, 'b) t
val hash : 'a -> int
val seeded_hash : int -> 'a -> int
val hash_param : int -> int -> 'a -> int
val seeded_hash_param : int -> int -> int -> 'a -> int

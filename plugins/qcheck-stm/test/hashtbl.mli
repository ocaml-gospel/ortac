type (!'a, !'b) t
(*@ mutable model contents : ('a * 'b) list *)

val create :
  ?random:(* thwart tools/sync_stdlib_docs *) bool -> int -> ('a, 'b) t
(*@ h = create ?random size
    ensures h.contents = [] *)

val clear : ('a, 'b) t -> unit
(*@ clear h
    modifies h
    ensures h.contents = [] *)

val reset : ('a, 'b) t -> unit
(*@ reset h
    modifies h
    ensures h.contents = [] *)

val copy : ('a, 'b) t -> ('a, 'b) t
(*@ h2 = copy h1
    ensures h2.contents = h1.contents *)

val add : ('a, 'b) t -> 'a -> 'b -> unit
(*@ add h a b
    modifies h
    ensures h.contents = (a, b) :: old h.contents *)

val find : ('a, 'b) t -> 'a -> 'b
(*@ b = find h a
    raises Not_found -> forall x. not (List.mem (a, x) h.contents)
    raises Not_found -> not (List.mem a (List.map fst h.contents))
    ensures List.mem (a, b) h.contents *)

val find_opt : ('a, 'b) t -> 'a -> 'b option
(*@ o = find_opt h a
    ensures match o with
      | None -> not (List.mem a (List.map fst h.contents))
      | Some b -> List.mem (a, b) h.contents *)

val find_all : ('a, 'b) t -> 'a -> 'b list
(*@ bs = find_all h a
    ensures bs = Sequence.filter_map (fun (x, y) -> if x = a then Some y else None) h.contents *)

val mem : ('a, 'b) t -> 'a -> bool
(*@ b = mem h a
    ensures b = List.mem a (List.map fst h.contents) *)

(*@ function rec remove_first (x: 'a) (xs : ('a * 'b) list) : ('a * 'b) list =
      match xs with
      | (a, b) :: xs -> if a = x then xs else (a, b) :: (remove_first x xs)
      | [] -> [] *)

val remove : ('a, 'b) t -> 'a -> unit
(*@ remove h a
    modifies h
    ensures h.contents = remove_first a (old h.contents) *)

val replace : ('a, 'b) t -> 'a -> 'b -> unit
(*@ replace h a b
    modifies h
    ensures h.contents = (a, b) :: remove_first a (old h.contents) *)

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
    ensures i = List.length h.contents *)

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

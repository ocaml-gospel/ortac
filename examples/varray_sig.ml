module type ARRAY = sig
  (** The array will be used partially, with some elements in an undefined
      state.  It is guaranteed that all operations always use valid indexes.
  *)

  type 'a t
  (** The type of an array. *)

  type 'a elt
  (** The type of elements contained in the array. *)

  val length : 'a t -> int
  (** The size of the array; the maximum number of elements that can be stored
      inside. *)

  val empty : unit -> 'a t
  (** An empty array of length [0]. *)

  val create : int -> 'a t
  (** [create n] returns an array of length [n]. Its elements are all in
      an undefined state and will not be accessed by {! get} before being
      defined by {! set} or {! blit}. *)

  val get : 'a t -> int -> 'a elt
  (** [get t i] returns the [i]th element of the array.
      The elements are numbered from [0] to [length t - 1] and the index [i] is
      always within this bound: this function be implemented as an
      [unsafe_get]) if available. *)

  val set : 'a t -> int -> 'a elt -> unit
  (** [set t i v] modifies [t] in place, replacing the element at position [i]
      with the value [v]. From now on, the element at this index is defined.
      Again, this can be implemented as an [unsafe_set] without bound checking.
  *)

  val erase_at : 'a t -> int -> unit
  (** [erase_at t i] resets the element at position [i]. It is an opportunity
      to free the memory of the value [t.(i)]. From now on, the element is
      undefined and this index will not be accessed again until a write is
      done. *)

  val blit : 'a t -> int -> 'a t -> int -> int -> unit
  (** [blit src i dst j n] copies the elements from the range [i,i+n-1] from
      the array [src] to the range [j,j+n-1] of the array [dst]. All the
      elements copied from [src] are guaranteed to be in a defined state.
      After this operation, the corresponding range in [dst] will be defined.
      The copied ranges will be valid for each array. Special care is required
      during the copy since [src] will often be the same array as [dst] and the
      ranges can overlap. *)
end

module type TIER = sig
  module Array : ARRAY
  type 'a elt = 'a Array.elt
  type 'a array = 'a Array.t

  type 'a t

  val depth : int

  val empty : unit -> 'a t
  val is_empty : 'a t -> bool
  val root_capacity : 'a t -> int

  val create : capacity:int -> 'a t
  val make : lc:int -> int -> 'a elt -> 'a t
  val init : lc:int -> offset:int -> int -> (int -> 'a elt) -> 'a t

  val length : 'a t -> int
  val capacity : lc:int -> int

  val get : lc:int -> 'a t -> int -> 'a elt
  val set : lc:int -> 'a t -> int -> 'a elt -> unit

  val pop_front : lc:int -> 'a t -> 'a elt
  val pop_back : lc:int -> 'a t -> 'a elt
  val pop_at : lc:int -> 'a t -> int -> 'a elt

  val push_front : lc:int -> 'a t -> 'a elt -> unit
  val push_back : lc:int -> 'a t -> 'a elt -> unit

  val is_full : lc:int -> 'a t -> bool

  val push_front_pop_back : lc:int -> 'a t -> 'a elt -> 'a elt
  val push_back_pop_front : lc:int -> 'a t -> 'a elt -> 'a elt

  val insert_at : lc:int -> 'a t -> int -> 'a elt -> unit
end

module type VARRAY = sig
  type 'a t
  type 'a elt
  type 'a array

  val push_back : 'a t -> 'a elt -> unit
  val pop_back : 'a t -> 'a elt

  val push_front : 'a t -> 'a elt -> unit
  val pop_front : 'a t -> 'a elt

  val insert_at : 'a t -> int -> 'a elt -> unit
  val pop_at : 'a t -> int -> 'a elt
  val delete_at : 'a t -> int -> unit

  val get : 'a t -> int -> 'a elt
  val set : 'a t -> int -> 'a elt -> unit

  val length : 'a t -> int
  val make : int -> 'a elt -> 'a t
  val init : int -> (int -> 'a elt) -> 'a t
  val empty : unit -> 'a t
  val is_empty : 'a t -> bool

  val protect : 'a t -> (unit -> 'b) -> 'b

  module Tier : TIER with type 'a Array.elt = 'a elt
                      and type 'a Array.t = 'a array
end

module type S = sig

  (** inside is a helper function to check that an index is not out of bound *)
  (*@ function inside (i : integer) (s : 'a sequence) : bool =
        0 <= i < Sequence.length s *)

  type 'a t
  (** The type of a varray. *)
  (*@ mutable model contents : 'a sequence *)

  type 'a elt = 'a
  (** The type of elements stored in the varray. *)

  (** A projection function to read the element of the array *)
  (*@ function proj (e : 'a elt) : 'a = e *)

  (** {1 Dynamic collection} *)

  val push_back : 'a t -> 'a elt -> unit
  (** [push_back t x] adds a new element [x] at the end of the varray [t].
      {b O(k)} amortized. *)
  (*@ push_back t x
      modifies t.contents
      ensures t.contents = Sequence.snoc (old t.contents) (proj x) *)

  val pop_back : 'a t -> 'a elt
  (** [pop_back t] removes and returns the rightmost element of the varray [t].
      {b O(k)} amortized. *)
  (*@ x = pop_back t
      modifies t.contents
      ensures t.contents = if old t.contents = Sequence.empty
                           then Sequence.empty
                           else (old t.contents)[..Sequence.length (old t.contents) - 2]
      ensures if old t.contents = Sequence.empty
              then false
              else proj x = (old t.contents)[Sequence.length (old t.contents) - 1]
      raises Not_found -> t.contents = old t.contents = Sequence.empty *)

  val push_front : 'a t -> 'a elt -> unit
  (** [push_front t x] inserts a new element [x] at position [0], on the left
      side of the varray [t]. Every previous element of [t] is shifted one to
      the right. {b O(k)} amortized. *)
  (*@ push_front t x
      modifies t.contents
      ensures t.contents = Sequence.cons (proj x) (old t.contents) *)


  val pop_front : 'a t -> 'a elt
  (** [pop_front t] removes and returns the leftmost element at position [0] of
      the varray [t]. Every element of [t] is shifted one to the right.
      {b O(k)} amortized. *)
  (*@ x = pop_front t
      modifies t.contents
      ensures t.contents = if old t.contents = Sequence.empty
                           then Sequence.empty
                           else Sequence.tl (old t.contents)
      ensures if old t.contents = Sequence.empty
              then false
              else proj x = Sequence.hd (old t.contents)
      raises Not_found -> t.contents = old t.contents = Sequence.empty *)

  val insert_at : 'a t -> int -> 'a elt -> unit
  (** [insert_at t i x] inserts the element [x] at position [i] in the varray
      [t]. Every element on the right of [i] is shifted by one.
      {b O(k² × {%html:<sup>k</sup>%}√N)}

      - [insert_at t 0 x] is the same as [push_front t x]
      - [insert_at t (length t) x] is the same as [push_back t x]

      @raise Invalid_arg if [i] is negative or larger than [length t].
  *)
  (*@ insert_at t i x
      checks 0 <= i <= Sequence.length t.contents
      modifies t.contents
      ensures t.contents = old (if 0 <= i <= Sequence.length t.contents
                                then t.contents[..i - 1] ++ (Sequence.cons (proj x) t.contents[i..])
                                else t.contents) *)

  val pop_at : 'a t -> int -> 'a elt
  (** [pop_at t i] removes and returns the element [t.(i)]. Every element on
      the right of [i] is shifted by one to the left.
      {b O(k² × {%html:<sup>k</sup>%}√N)}

      - [pop_at t 0] is the same as [pop_front t]
      - [pop_at t (length t - 1)] is the same as [pop_back t]

      @raise Invalid_arg if [i] is negative or larger than [length t - 1].
  *)
  (*@ x = pop_at t i
      checks inside i t.contents
      modifies t.contents
      ensures t.contents = old (t.contents[..i - 1] ++ t.contents[(i + 1)..])
      ensures (proj x) = old t.contents[i] *)

  val delete_at : 'a t -> int -> unit
  (** [delete_at t i] removes the element [t.(i)]. Every element on the right
      of [i] is shifted by one to the left.
      {b O(k² × {%html:<sup>k</sup>%}√N)}

      @raise Invalid_arg if [i] is negative or larger than [length t - 1].
  *)
  (*@ delete_at t i
      checks inside i t.contents
      modifies t.contents
      ensures t.contents = old (t.contents[..i - 1] ++ t.contents[(i + 1)..])
      ensures Sequence.length t.contents = Sequence.length (old t.contents) - 1 *)

  (** {1 Freeze during traversals} *)

  (** The previous operations all fail when the varray is being traversed: *)

  val protect : 'a t -> (unit -> 'b) -> 'b
  (** [protect t fn] marks [t] as protected during the execution of [fn ()].
      All operations that would update the length of [t] by pushing or poping
      elements will raise a [Failure] indicating that the traversal is unsafe.
  *)

  (** {1 Array} *)

  val get : 'a t -> int -> 'a elt
  (** [get t i] returns the [i]th element of the varray. Indexing starts from
      [0] upto [length t - 1]. {b O(k)}

      @raise Invalid_argument if [i] is negative
      or larger than [length t - 1].
  *)
  (*@ x = get t i
      ensures (proj x) = t.contents[i]
      checks inside i t.contents *)

  val set : 'a t -> int -> 'a elt -> unit
  (** [set t i v] updates the value of the [i]th element to [x]. {b O(k)}

      @raise Invalid_argument if [i] is negative
      or larger than [length t - 1].
  *)
  (*@ set t i v
      checks inside i t.contents
      modifies t.contents
      ensures t.contents = Sequence.set (old t.contents) i (proj v) *)

  val length : 'a t -> int
  (** [length t] returns the number of elements stored in [t]. {b O(1)} *)
  (*@ l = length t
      ensures l = Sequence.length t.contents *)

  val make : int -> 'a elt -> 'a t
  (** [make n x] returns a new varray of length [n], where all the elements are
      initialized to the value [x].

      @raise Invalid_argument if [n] is negative.
  *)
  (*@ t = make n x
      checks n >= 0
      ensures t.contents = Sequence.init n (fun _ -> proj x) *)

  val init : int -> (int -> 'a elt) -> 'a t
  (** [init n f] returns a new array of length [n], where the element at
      position [i] is initialized to [f i].

      @raise Invalid_argument if [n] is negative.
  *)

  val empty : unit -> 'a t
  (** [empty ()] is a new varray of length [0]. *)
  (*@ t = empty ()
      ensures t.contents = Sequence.empty *)

  val is_empty : 'a t -> bool
  (** [is_empty t] returns true when the varray [t] has length [0]. *)
  (*@ b = is_empty t
      ensures b <-> t.contents = Sequence.empty *)

  (** {1 Copying elements} *)

  val append : 'a t -> 'a t -> 'a t
  (** [append a b] returns a new varray by concatening the elements of [a] with
      those of [b]. *)
  (*@ t = append a b
      ensures t.contents = a.contents ++ b.contents *)

  val concat : 'a t list -> 'a t
  (** [concat ts] returns a new varray whose elements are in the same order as
      the values from the list of varrays [ts]. *)
  (*@ t = concat ts
      ensures t.contents = List.fold_left (fun acc s -> acc ++ s.contents) Sequence.empty ts *)

  val sub : 'a t -> int -> int -> 'a t
  (** [sub t i n] returns a new varray of length [n], containing the elements
      from the range [i, i+n-1] of the varray [t].

      @raise Invalid_argument if the range [i, i + n - 1] is invalid for [t].
  *)
  (*@ r = sub t i n
      checks 0 <= i <= Sequence.length t.contents
      checks i <= i + n <= Sequence.length t.contents
      ensures r.contents = if n = 0 then Sequence.empty else t.contents[i..i+n-1] *)

  val copy : 'a t -> 'a t
  (** [copy t] returns a new varray containing the same sequence of
      elements as [t]. *)
  (*@ r = copy t
      ensures r.contents = t.contents *)

  val fill : 'a t -> int -> int -> 'a elt -> unit
  (** [fill t pos len x] modifies the varray [t] in place, by setting the value
      [x] in the range [pos, pos + len - 1].

      @raise Invalid_argument if the range [pos, pos + len -1] is invalid.
  *)
  (*@ fill t pos len x
      checks 0 <= pos /\ 0 <= len /\ pos + len < Sequence.length t.contents
      modifies t.contents
      ensures t.contents = Sequence.init (Sequence.length (old t.contents)) (fun i -> if pos <= i < pos + len then proj x else (old t.contents)[i]) *)

  val blit : 'a t -> int -> 'a t -> int -> int -> unit
  (** [blit src src_pos dst dst_pos len] updates the varray [dst] in place, by
      copying the range [src_pos, src_pos + len - 1] of values from [src] into
      the destination range [dst_pos, dst_pos + len - 1] of [dst].

      @raise Invalid_argument if the ranges are invalid for either varray.
  *)
  (*@ blit src src_pos dst dst_pos len
      checks 0 <= src_pos <= src_pos + len <= Sequence.length src.contents
      checks 0 <= dst_pos <= dst_pos + len <= Sequence.length dst.contents
      modifies dst.contents
      ensures dst.contents =
        if dst_pos = 0 then old (src.contents[src_pos..src_pos + len - 1] ++ dst.contents [len..])
        else old (dst.contents[..dst_pos-1] ++ src.contents[src_pos..src_pos + len - 1]
            ++ dst.contents[dst_pos + len..]) *)

  (** {1 Traversals} *)

  val iter : ('a elt -> unit) -> 'a t -> unit
  (** [iter f t] calls the function [f] on all elements of [t], from left to
      right. *)

  val iteri : (int -> 'a elt -> unit) -> 'a t -> unit
  (** [iteri f t] calls [f i t.(i)] on all the indexes [i] of [t],
      from left to right. *)

  val map : ('a elt -> 'b elt) -> 'a t -> 'b t
  (** [map f t] returns a new varray, whose elements are [f x] for each [x]
      from the varray [t]. *)

  val mapi : (int -> 'a elt -> 'b elt) -> 'a t -> 'b t
  (** [mapi f t] returns a new varray, whose elements are [f i t.(i)] for each
      index [i] of the varray [t]. *)

  val fold_left : ('a -> 'b elt -> 'a) -> 'a -> 'b t -> 'a
  (** [fold_left f z t] computes
      [f (... (f (f z t.(0)) t.(1)) ...) t.(length t - 1)]. *)

  val fold_right : ('a elt -> 'b -> 'b) -> 'a t -> 'b -> 'b
  (** [fold_right f t z] computes
      [f t.(0) (f t.(1) (... (f z t.(length t - 1))))]. *)

  val fold_left_map : ('a -> 'b elt -> 'a * 'c elt) -> 'a -> 'b t -> 'a * 'c t
  (** [fold_left_map] is a combination of [fold_left] and [map],
      that threads an accumulator. *)

  (** {1 Iterators on two varrays} *)

  val iter2 : ('a elt -> 'b elt -> unit) -> 'a t -> 'b t -> unit
  (** [iter2 f xs ys] calls [f xs.(i) ys.(i)] for each index [i] from left to
      right.

      @raise Invalid_argument if the two varrays have different lengths.
  *)

  val map2 : ('a elt -> 'b elt -> 'c elt) -> 'a t -> 'b t -> 'c t
  (** [map2 f xs ys] returns a new varray whose [i]th element is
      [f xs.(i) ys.(i)].

      @raise Invalid_argument if the two varrays have different lengths.
  *)

  (** {1 Predicates} *)

  val for_all : ('a elt -> bool) -> 'a t -> bool
  (** [for_all f t] holds when [f] is satisfied by all the elements of [t]. *)

  val for_all2 : ('a elt -> 'b elt -> bool) -> 'a t -> 'b t -> bool
  (** [for_all2 f xs ys] holds when [f xs.(i) ys.(i)] is satisfied by all
      indexes [i].

      @raise Invalid_argument if the two varrays have different lengths.
  *)

  val exists : ('a elt -> bool) -> 'a t -> bool
  (** [exists f t] holds when [f] is satisfied by one of the elements of
      [t]. *)

  val exists2 : ('a elt -> 'b elt -> bool) -> 'a t -> 'b t -> bool
  (** [exists2 f xs ys] holds when an index [i] exists such that
      [f xs.(i) ys.(i)] is satisfied.

      @raise Invalid_argument if the two varrays have different lengths.
  *)

  val find_opt : ('a elt -> bool) -> 'a t -> 'a elt option
  (** [find_opt f t] returns the leftmost element of [t] that satisfies [f]. *)

  val find_map : ('a elt -> 'b option) -> 'a t -> 'b option
  (** [find_map f t] returns the first result of [f] of the form [Some v]. *)

  val mem : 'a elt -> 'a t -> bool
  (** [mem x t] is true when [x] is equal [( = )] to an element of the varray
      [t]. *)

  val memq : 'a elt -> 'a t -> bool
  (** Same as [mem], but [memq x t] uses physical equality [( == )] for
      comparison. *)

  (** {1 Sort} *)

  val sort : ('a elt -> 'a elt -> int) -> 'a t -> unit
  (** [sort cmp t] updates [t] inplace by sorting the elements in increasing
      order according to [cmp]. *)

  val stable_sort : ('a elt -> 'a elt -> int) -> 'a t -> unit
  (** Same as [sort], but equal elements are kept in the same relative
      order. *)

  val fast_sort : ('a elt -> 'a elt -> int) -> 'a t -> unit
  (** Same as [sort]. *)

  (** {1 Conversions} *)

  type 'a array
  (** The array type used behind the scene as a backend by the varray. *)

  val of_array : 'a array -> 'a t
  (** [of_array arr] returns a new varray containing all the elements of the
      array [arr]. *)

  val to_array : 'a t -> 'a array
  (** [to_array t] returns a new array containing all the elements of the
      varray [t]. *)

  val of_list : 'a elt list -> 'a t
  (** [of_list xs] returns a new varray containing all the elements of the list
      [xs]. *)

  val to_list : 'a t -> 'a elt list
  (** [to_list t] returns a list of all the elements of [t]. *)
end

type 'a t
(*@ mutable model contents : 'a sequence *)

exception Empty

val create : unit -> 'a t
(*@ t = create ()
    ensures t.contents = Sequence.empty *)

val push : 'a -> 'a t -> unit
(*@ push v t
    modifies t.contents
    ensures t.contents = Sequence.snoc (old t.contents) v *)

val pop : 'a t -> 'a
(*@ v = pop t
    raises Empty -> t.contents = old t.contents = Sequence.empty
    modifies t.contents
    ensures t.contents = match Sequence.length (old t.contents) with
        | 0 -> Sequence.empty
        | _ -> Sequence.tl (old t.contents)
    ensures v = Sequence.hd (old t.contents)
    ensures old t.contents <> Sequence.empty *)

val peek : 'a t -> 'a
(*@ v = peek t
    raises Empty -> t.contents = old t.contents = Sequence.empty
    ensures v = Sequence.hd t.contents *)

val peek_opt : 'a t -> 'a option
(*@ v = peek_opt t
    ensures match v with
        | None -> t.contents = Sequence.empty
        | Some a -> a = Sequence.hd t.contents *)

val clear : 'a t -> unit
(*@ clear t
    modifies t.contents
    ensures t.contents = Sequence.empty *)

val is_empty : 'a t -> bool
(*@ b = is_empty t
    ensures b = match Sequence.length t.contents with
        | 0 -> true
        | _ -> false *)

val transfer : 'a t -> 'a t -> unit
(*@ transfer t1 t2
    modifies t1.contents
    modifies t2.contents
    ensures t1.contents = Sequence.empty
    ensures t2.contents = Sequence.append (old t1.contents) (old t2.contents) *)

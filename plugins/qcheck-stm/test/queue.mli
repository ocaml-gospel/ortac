type 'a t
(*@ mutable model contents : 'a sequence *)

exception Empty

val create : unit -> 'a t
(*@ t = create ()
    ensures t.contents = Sequence.empty *)

val add : 'a -> 'a t -> unit
(*@ add v t
    modifies t.contents
    ensures t.contents = Sequence.snoc (old t.contents) v *)

val push : 'a -> 'a t -> unit
(*@ push v t
    modifies t.contents
    ensures t.contents = Sequence.snoc (old t.contents) v *)

val take : 'a t -> 'a
(*@ v = take t
    raises Empty -> t.contents = old t.contents = Sequence.empty
    modifies t.contents
    ensures t.contents = match Sequence.length (old t.contents) with
        | 0 -> Sequence.empty
        | _ -> Sequence.tl (old t.contents)
    ensures v = Sequence.hd (old t.contents)
    ensures old t.contents <> Sequence.empty *)

val take_opt : 'a t -> 'a option
(*@ r = take_opt t
    modifies t.contents
    ensures t.contents = if Sequence.length (old t.contents) = 0 then
        old t.contents else Sequence.tl (old t.contents)
    ensures r = if (old t.contents) = Sequence.empty then
        None else Some (Sequence.hd (old t.contents)) *)

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

val top : 'a t -> 'a
(*@ v = top t
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

val copy : 'a t -> 'a t
(*@ r = copy t
    ensures r.contents = t.contents *)

val is_empty : 'a t -> bool
(*@ b = is_empty t
    ensures b = match Sequence.length t.contents with
        | 0 -> true
        | _ -> false *)

val length : 'a t -> int
(*@ l = length t
    ensures l = Sequence.length t.contents *)

val transfer : 'a t -> 'a t -> unit
(*@ transfer t1 t2
    modifies t1.contents
    modifies t2.contents
    ensures t1.contents = Sequence.empty
    ensures t2.contents = Sequence.append (old t1.contents) (old t2.contents) *)

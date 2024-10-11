type 'a t
(*@ mutable model contents : 'a sequence *)

exception Empty

val create : unit -> 'a t
(*@ t = create ()
    ensures t.contents = Sequence.empty *)

val push : 'a -> 'a t -> unit
(*@ push v t
    modifies t.contents
    ensures t.contents = Sequence.cons v (old t.contents) *)

val pop : 'a t -> 'a
(*@ v = pop t
    modifies t.contents
    raises Empty -> t.contents = old t.contents = Sequence.empty
    ensures t.contents = if Sequence.length (old t.contents) = 0 then
        (old t.contents) else Sequence.tl (old t.contents)
    ensures v = Sequence.hd (old t.contents)
    ensures old t.contents <> Sequence.empty *)

val pop_opt : 'a t -> 'a option
(*@ v = pop_opt t
    modifies t.contents
    ensures t.contents = if Sequence.length (old t.contents) = 0 then
        old t.contents else Sequence.tl (old t.contents)
    ensures v = if (old t.contents) = Sequence.empty then
        None else Some (Sequence.hd (old t.contents)) *)

val top : 'a t -> 'a
(*@ v = top t
    raises Empty -> t.contents = old t.contents = Sequence.empty
    ensures v = Sequence.hd t.contents *)

val top_opt : 'a t -> 'a option
(*@ v = top_opt t
    ensures match v with
        | None -> t.contents = Sequence.empty
        | Some x -> x = Sequence.hd t.contents *)

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

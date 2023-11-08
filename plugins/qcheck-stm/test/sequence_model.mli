type 'a t
(*@ mutable model contents : 'a sequence *)

val create : unit -> 'a t
(*@ t = create ()
    ensures t.contents = Sequence.empty *)

val add : 'a -> 'a t -> unit
(*@ add v t
    modifies t.contents
    ensures t.contents = Sequence.cons v (old t.contents) *)

(* Just for test purpose *)
(*@ function length_opt (s : 'a sequence) : integer option =
      Some (Sequence.length s) *)

val remove : 'a t -> 'a option
(*@ o = remove t
    modifies t.contents
    ensures t.contents = match Sequence.length (old t.contents) with
                          | 0 -> Sequence.empty
                          | _ -> Sequence.tl (old t.contents) *)

val remove_ : 'a t -> 'a option
(*@ o = remove_ t
    modifies t.contents
    ensures t.contents = match length_opt (old t.contents) with
                          | Some 0 -> Sequence.empty
                          | _ -> Sequence.tl (old t.contents) *)

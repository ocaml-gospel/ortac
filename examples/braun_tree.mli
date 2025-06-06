type 'a t
(*@ model size: int
    model cont: 'a list *)

val size : 'a t -> int [@@model]
val cont : 'a t -> 'a list [@@model]
val get : 'a t -> int -> 'a
(*@ x = get t i
    (* requires t.cont <> [] *)
    ensures x = List.nth t.cont i *)

val head : 'a t -> 'a
(*@ x = head t
    (* requires t.cont <> [] *)
    ensures x = List.hd t.cont *)

val left_t : 'a t -> 'a t
(*@ t' = left_t t
    pure
    (* requires t.cont <> [] *)
*)

val right_t : 'a t -> 'a t
(*@ t' = right_t t
    pure
    (* requires t.cont <> [] *)
*)

val empty : unit -> 'a t
(** Create an empty Braun tree *)
(*@ t = empty ()
    ensures t.size = 0
    ensures t.cont = [] *)

val cons : 'a -> 'a t -> 'a t
(** [cons x t] returns [t] with [x] as new root. *)
(*@ t' = cons x t
    ensures t'.size = t.size + 1
    ensures List.hd t'.cont = x
    ensures t'.cont = x :: t.cont *)

val tail : 'a t -> 'a t
(** [tail t] removes the first element of [t] and returns the popped element and
    the updated Braun tree. *)
(*@ t' = tail t
    (* requires t.cont <> [] *)
    ensures t.size = t'.size + 1 *)

val snoc : 'a -> 'a t -> 'a t
(** [snoc x t] returns [t] with [x] appended at the end. *)
(*@ t' = snoc x t
    ensures t'.size = t.size + 1
    ensures List.hd t'.cont = List.hd t.cont
    ensures List.rev t'.cont = x :: List.rev t.cont *)

val liat : 'a t -> 'a t
(** [tail t] removes the last element of [t] and returns the popped element and
    the updated Braun tree. *)
(*@ t' = liat t
    (* requires t.cont <> [] *)
    ensures t.size = t'.size + 1
    *)

val to_list : 'a t -> 'a list
val of_list : 'a list -> 'a t

(* val find: int -> 'a t -> 'a
(** [find n t] returns the value at index [n]. *)
(*@ x = find i t
    pure
    (* requires i < t.size *)
    ensures x = List.nth t.cont i *) *)

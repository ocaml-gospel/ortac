type 'a t
(*@ model size: int
    model cont: 'a list *)

val size: 'a t -> int     [@@model]
val cont: 'a t -> 'a list [@@model]

val empty: unit -> 'a t
(** Create an empty Braun tree *)
(*@ t = empty ()
    ensures t.size = 0
    ensures t.cont = [] *)

val add_front: 'a -> 'a t -> 'a t
(** [add_front x t] returns [t] with [x] as new root. *)
(*@ t' = add_front x t
    ensures t'.size = t.size + 1
    ensures t'.cont = x :: t.cont *)

val remove_first: 'a t -> 'a * 'a t
(** [remove_first t] removes the first element of [t] and returns the popped element and the updated Braun tree. *)
(*@ x, t' = remove_first t
    requires t.cont <> []
    ensures t.size = t'.size + 1
    ensures t.cont = x :: t'.cont *)

val append: 'a -> 'a t -> 'a t
(** [append x t] returns [t] with [x] appended at the end. *)
(*@ t' = append x t
    ensures t'.size = t.size + 1
    ensures List.rev t'.cont = x :: List.rev t.cont *)

val remove_last: 'a t -> 'a * 'a t
(** [remove_first t] removes the last element of [t] and returns the popped element and the updated Braun tree. *)
(*@ x, t' = remove_last t
    requires t.cont <> []
    ensures t.size = t'.size + 1
    ensures List.rev t.cont = x :: List.rev t'.cont *)

val find: int -> 'a t -> 'a
(** [find n t] returns the value at index [n]. *)
(*@ x = find i t
    pure
    requires i < t.size
    ensures x = List.nth t.cont i *)

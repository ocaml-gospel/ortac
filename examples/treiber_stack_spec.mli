(** Lock-free multi-producer multi-consumer Treiber stack.

    All functions are lock-free. It is the recommended starting point when
    needing a LIFO structure. *)

(** {1 API} *)

(*@ predicate seq_is_empty (s : 'a sequence) =
       s = Sequence.empty *)

type 'a t
(** Represents a lock-free Treiber stack holding elements of type ['a]. *)
(*@ mutable model contents : 'a sequence *)

val create : unit -> 'a t
(** [create ()] creates a new empty Treiber stack. *)
(*@ a = create ()
    ensures a.contents = Sequence.empty *)

val of_list : 'a list -> 'a t
(** [of_list list] creates a new Treiber stack from a list. *)
(*@ a = of_list xs
    ensures a.contents = List.to_seq xs *)

val is_empty : 'a t -> bool
(** [is_empty stack] returns [true] if the [stack] is empty, otherwise [false].
*)
(*@ b = is_empty a
    ensures b = seq_is_empty a.contents *)

(** {2 Consumer functions} *)

exception Empty
(** Raised when {!pop_exn}, {!peek_exn} and {!drop_exn} is applied to an empty
    stack. *)

val peek_exn : 'a t -> 'a
(** [peek_exn stack] returns the top element of the [stack] without removing it.

    @raise Empty if the [stack] is empty. *)

val peek_opt : 'a t -> 'a option
(** [peek_opt stack] returns [Some] of the top element of the [stack] without
    removing it, or [None] if the [stack] is empty. *)
(*@ o = peek_opt a
    ensures o = if seq_is_empty a.contents
                then None
                else Some (Sequence.hd a.contents) *)

val pop_exn : 'a t -> 'a
(** [pop_exn stack] removes and returns the top element of the [stack].

    @raise Empty if the [stack] is empty. *)

val pop_opt : 'a t -> 'a option
(** [pop_opt stack] removes and returns [Some] of the top element of the
    [stack], or [None] if the [stack] is empty. *)
(*@ o = pop_opt a
    modifies a
    ensures a.contents = if seq_is_empty (old a.contents)
                         then Sequence.empty
                         else Sequence.tl (old a.contents)
    ensures o = if seq_is_empty (old a.contents)
                then None
                else Some (Sequence.hd (old a.contents)) *)

val drop_exn : 'a t -> unit
(** [drop_exn stack] removes the top element of the [stack].

    @raise Empty if the [stack] is empty. *)

val pop_all : 'a t -> 'a list
(** [pop_all stack] removes and returns all elements of the [stack] in LIFO
    order.

    {[
      # open Saturn.Stack
      # let t : int t = create ()
      val t : int t = <abstr>
      # push t 1
      - : unit = ()
      # push t 2
      - : unit = ()
      # push t 3
      - : unit = ()
      # pop_all t
      - : int list = [3; 2; 1]
    ]} *)
(*@ xs = pop_all a
    modifies a.contents
    ensures a.contents = Sequence.empty
    ensures xs = List.of_seq (old a.contents) *)

(** {2 Producer functions} *)

val push : 'a t -> 'a -> unit
(** [push stack element] adds [element] to the top of the [stack]. *)
(*@ push a x
    modifies a.contents
    ensures a.contents = Sequence.cons x (old a.contents) *)

val push_all : 'a t -> 'a list -> unit
(** [push_all stack elements] adds all [elements] to the top of the [stack].

    🐌 This is a linear-time operation on the size of [elements].

    {[
      # let t : int t = create ()
      val t : int t = <abstr>
      # push_all t [1; 2; 3; 4]
      - : unit = ()
      # pop_opt t
      - : int option = Some 4
      # pop_opt t
      - : int option = Some 3
      # pop_all t
      - : int list = [2; 1]
    ]} *)
(*@ push_all a xs
    modifies a.contents
    ensures a.contents = Sequence.rev (List.to_seq xs) ++ old a.contents *)

(** {2 With Sequences}*)
val to_seq : 'a t -> 'a Seq.t
(** [to_seq stack] takes a snapshot of [stack] and returns its value top to
    bottom.

    🐌 This is a linear time operation. *)

val of_seq : 'a Seq.t -> 'a t
(** [of_seq seq] creates a stack from a [seq]. It must be finite.

    🐌 This is a linear-time operation. *)

val add_seq : 'a t -> 'a Seq.t -> unit
(** [add_seq stack seq] adds all elements of [seq] to the top of the [stack].
    [seq] must be finite.

    🐌 This is a linear-time operation on the size of [elements]. *)

(** {1 Examples} *)

(** {2 Sequential example}
    An example top-level session:
    {[
      # open Saturn.Stack
      # let t : int t = create ()
      val t : int t = <abstr>
      # push t 42
      - : unit = ()
      # push_all t [1; 2; 3]
      - : unit = ()
      # pop_exn t
      - : int = 3
      # peek_opt t
      - : int option = Some 2
      # pop_all t
      - : int list = [2; 1; 42]
      # pop_exn t
      Exception: Saturn__Treiber_stack.Empty.
    ]} *)

(** {2 Multicore example}
    Note: The barrier is used in this example solely to make the results more
    interesting by increasing the likelihood of parallelism. Spawning a domain
    is a costly operation, especially compared to the relatively small amount of
    work being performed here. In practice, using a barrier in this manner is
    unnecessary.

    {@ocaml non-deterministic=command[
      # open Saturn.Stack
      # let t : int t = create ()
      val t : int t = <abstr>
      # let barrier =  Atomic.make 2
      val barrier : int Atomic.t = <abstr>

      # let pusher () =
          Atomic.decr barrier;
          while Atomic.get barrier != 0 do Domain.cpu_relax () done;
          push_all t [1;2;3] |> ignore;
          push t 42;
          push t 12
      val pusher : unit -> unit = <fun>

      # let popper () =
          Atomic.decr barrier;
          while Atomic.get barrier != 0 do Domain.cpu_relax () done;
          List.init 6 (fun i -> Domain.cpu_relax (); pop_opt t)
      val popper : unit -> int option list = <fun>

      # let domain_pusher = Domain.spawn pusher
      val domain_pusher : unit Domain.t = <abstr>
      # let domain_popper = Domain.spawn popper
      val domain_popper : int option list Domain.t = <abstr>
      # Domain.join domain_pusher
      - : unit = ()
      # Domain.join domain_popper
      - : int option list = [Some 42; Some 3; Some 2; Some 1; None; Some 12]
    ]} *)

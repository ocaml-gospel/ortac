include module type of Ortac_runtime
open STM

(** This type carries the expected value computed from the Gospel specification
    if possible. *)
type expected_result =
  | Value of res  (** The value has been computed *)
  | Protected_value of res
      (** The value has been computed but is protected as it could have been an
          exception *)
  | Exception of string  (** An exception is expected *)
  | Out_of_domain
      (** The computation of the expected returned value called a Gospel
          function out of its domain *)

type report
(** Information for the bug report in case of test failure *)

val report :
  string ->
  string ->
  expected_result ->
  string ->
  (string * location) list ->
  report
(** [report module_name init_sut ret cmd terms] *)

val append : report option -> report option -> report option
(** [append a b] appends the violated terms of [a] and [b] if any in the
    returned report *)

val dummy : 'a ty * ('b -> string)
(** A dummy [STM.res] for unknown returned values *)

module Model : sig
  module Make (M : sig
    type elt

    val init : elt
  end) : sig
    type elt
    (** The type of a model of a single state *)

    type t
    (** A value of type [t] represents an immutable model of a stack of states
    *)

    val create : int -> unit -> t
    (** [create n ()] creates a model with [n] initial elements *)

    val size : t -> int
    (** [size t] returns the number of elements in the model [t] *)

    val drop_n : t -> int -> t
    (** [drop_n t n] returns a new model with the [n] uppermost elements removed
    *)

    val push : t -> elt -> t
    (** [push t e] returns a new model with [e] as the topmost element *)

    val get : t -> int -> elt
    (** [get t n] returns the [n]th element of the model [t] *)
  end
  with type elt := M.elt
end

module SUT : sig
  module Make (M : sig
    type sut

    val init : unit -> sut
  end) : sig
    type elt
    (** Type of a single SUT *)

    type t
    (** Values of type [t] represent a stack of SUTs *)

    val create : int -> unit -> t
    (** [create n ()] creates an initial stack with [n] SUTs *)

    val size : t -> int
    (** [size t] returns the number of SUTs currently on the stack [t] *)

    val pop : t -> elt
    (** [pop t] pops and returns the topmost element of the stack [t] of SUTs *)

    val get : t -> int -> elt
    (** [get t i] gets the i-th SUT in the store *)

    val push : t -> elt -> unit
    (** [push t e] pushes the SUT e onto the stack [t] of SUTs *)

    val get_name : t -> int -> string
    (** [get_name t n] returns the name for the [n]th element of the stack [t]
        of SUTs *)
  end
  with type elt = M.sut
end

module Make (Spec : Spec) : sig
  open QCheck

  val agree_test :
    count:int ->
    name:string ->
    int ->
    (unit -> unit) ->
    (Spec.cmd -> Spec.sut -> bool -> res -> string) ->
    (Spec.cmd -> Spec.state -> res -> report option) ->
    Test.t
  (** [agree_test ~count ~name max_suts init_state show_cmd postcond] An
      agreement test specialised to be used by Ortac/QCheck-STM. *)
end

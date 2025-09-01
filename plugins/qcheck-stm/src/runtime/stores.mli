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

    val get_name : t -> int -> string
    (** [get_name t n] returns the name for the [n]th element of the stack [t]
        of SUTs *)
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

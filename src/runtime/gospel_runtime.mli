type error =
  | BadPost of { loc : Ppxlib.location; fun_name : string; term : string }

val report : Format.formatter -> error -> unit

exception Error of error

module Z : sig
  include module type of Z

  val forall : t -> t -> (t -> bool) -> bool
  (** [forall i j p] is [true] iff the predicate `p` holds forall [k] within [i]
     and [j], included. *)
end

module Array : sig
  val create : Z.t -> 'a -> 'a array

  val get : 'a array -> Z.t -> 'a

  val length : 'a array -> Z.t
end

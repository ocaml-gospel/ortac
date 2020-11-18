module Z : sig
  include module type of Z

  val forall : t -> t -> (t -> bool) -> bool
  (** [forall i j p] is [true] iff the predicate `p` holds forall [k] within [i]
     and [j], included. *)
end

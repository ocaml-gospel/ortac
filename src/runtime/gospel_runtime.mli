type error =
  | BadPost of { loc : Ppxlib.location; fun_name : string; term : string }
  | RuntimeExn of {
      loc : Ppxlib.location;
      fun_name : string;
      term : string;
      exn : exn;
    }

val report : Format.formatter -> error -> unit

exception Error of error

val runtime_exn : Ppxlib.location -> string -> string -> exn -> 'a

val bad_post : Ppxlib.location -> string -> string -> 'a

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

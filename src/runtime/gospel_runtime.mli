type term = Pre of string | Post of string | XPost of string

type error_kind = Violated | RuntimeExn of exn

type error =
  | Condition of {
      loc : Ppxlib.location;
      fun_name : string;
      term : term;
      error_kind : error_kind;
    }
  | Unexpected_exception of {
      loc : Ppxlib.location;
      fun_name : string;
      exn : exn;
    }

val mk_condition : Ppxlib.location -> string -> term -> error_kind -> error

val mk_unexpected_exception : Ppxlib.location -> string -> exn -> error

val store : error -> error list ref -> unit

val report : Format.formatter -> error -> unit

val check_and_report : error list ref -> unit

val report_all : error list ref -> unit

exception Error of error list

val runtime_exn : Ppxlib.location -> string -> term -> exn -> 'a

val violated : Ppxlib.location -> string -> term -> 'a

val unexpected_exn : Ppxlib.location -> string -> exn -> 'a

module Z : sig
  include module type of Z

  val exists : t -> t -> (t -> bool) -> bool
  (** [exists i j p] is [true] iff the predicate there exists [k] within [i] and
      [j], included, for which [p] holds. *)

  val forall : t -> t -> (t -> bool) -> bool
  (** [forall i j p] is [true] iff the predicate `p` holds forall [k] within [i]
      and [j], included. *)
end

module Array : sig
  val create : Z.t -> 'a -> 'a array

  val get : 'a array -> Z.t -> 'a

  val length : 'a array -> Z.t
end

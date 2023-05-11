module type S = sig
  type location = { start : Lexing.position; stop : Lexing.position }
  type term_kind = Check | Pre | Post | XPost

  type error =
    | Violated_axiom
    | Axiom_failure of { exn : exn }
    | Violated_invariant of { term : string; position : term_kind }
    | Violated_condition of { term : string; term_kind : term_kind }
    | Specification_failure of {
        term : string;
        term_kind : term_kind;
        exn : exn;
      }
    | Unexpected_exception of { allowed_exn : string list; exn : exn }
    | Uncaught_checks of { term : string }
    | Unexpected_checks of { terms : string list }

  type error_report = {
    loc : location;
    fun_name : string;
    mutable errors : error list;
  }

  val pp_error_report : Format.formatter -> error_report -> unit

  exception Error of error_report

  module Errors : sig
    type t

    val create : location -> string -> t
    (** [empty] create a new empty error container *)

    val register : t -> error -> unit
    (** [register t a] add the element [a] to [t] *)

    val report : t -> unit
    (** [report l] prints the content of [l] *)
  end

  module Gospelstdlib : sig
    val ( ~! ) : 'a ref -> 'a
    val ( + ) : Z.t -> Z.t -> Z.t
    val ( - ) : Z.t -> Z.t -> Z.t
    val ( * ) : Z.t -> Z.t -> Z.t
    val ( / ) : Z.t -> Z.t -> Z.t
    val ( < ) : Z.t -> Z.t -> bool
    val ( <= ) : Z.t -> Z.t -> bool
    val ( > ) : Z.t -> Z.t -> bool
    val ( >= ) : Z.t -> Z.t -> bool
    val ( mod ) : Z.t -> Z.t -> Z.t
    val ( ~- ) : Z.t -> Z.t
    val abs : Z.t -> Z.t
    val logand : Z.t -> Z.t -> Z.t
    val max : Z.t -> Z.t -> Z.t
    val min : Z.t -> Z.t -> Z.t
    val pred : Z.t -> Z.t
    val succ : Z.t -> Z.t
    val pow : Z.t -> Z.t -> Z.t
    val integer_of_int : int -> Z.t

    module Array : sig
      val make : Z.t -> 'a -> 'a array
      val get : 'a array -> Z.t -> 'a
      val length : 'a array -> Z.t
      val for_all : ('a -> bool) -> 'a array -> bool
    end

    module List : sig
      val length : 'a list -> Z.t
      val hd : 'a list -> 'a
      val tl : 'a list -> 'a list
      val nth : 'a list -> Z.t -> 'a
      val rev : 'a list -> 'a list
      val init : Z.t -> (Z.t -> 'a) -> 'a list
      val map : ('a -> 'b) -> 'a list -> 'b list
      val mapi : (Z.t -> 'a -> 'b) -> 'a list -> 'b list
      val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
      val fold_right : ('b -> 'a -> 'a) -> 'b list -> 'a -> 'a
      val mem : 'a -> 'a list -> bool
    end
  end

  module Z : sig
    val exists : Z.t -> Z.t -> (Z.t -> bool) -> bool
    (** [exists i j p] is [true] iff the predicate there exists [k] within [i]
        and [j], included, for which [p] holds. *)

    val forall : Z.t -> Z.t -> (Z.t -> bool) -> bool
    (** [forall i j p] is [true] iff the predicate `p` holds forall [k] within
        [i] and [j], included. *)
  end
end

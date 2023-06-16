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

  type integer

  module Gospelstdlib : sig
    (** {1 Arithmetic} *)

    val succ : integer -> integer
    val pred : integer -> integer
    val ( ~- ) : integer -> integer
    val ( + ) : integer -> integer -> integer
    val ( - ) : integer -> integer -> integer
    val ( * ) : integer -> integer -> integer
    val ( / ) : integer -> integer -> integer
    val ( mod ) : integer -> integer -> integer
    val pow : integer -> integer -> integer
    val abs : integer -> integer
    val min : integer -> integer -> integer
    val max : integer -> integer -> integer

    (** {2 Comparisons} *)

    val ( > ) : integer -> integer -> bool
    val ( >= ) : integer -> integer -> bool
    val ( < ) : integer -> integer -> bool
    val ( <= ) : integer -> integer -> bool

    (** {2 Bitwise operations} *)

    val logand : integer -> integer -> integer

    (** {2 Machine integers} *)

    val integer_of_int : int -> integer

    (** {1 References} *)

    val ( ~! ) : 'a ref -> 'a

    (** {1 Lists} *)

    module List : sig
      val length : 'a list -> integer
      val hd : 'a list -> 'a
      val tl : 'a list -> 'a list
      val nth : 'a list -> integer -> 'a
      val rev : 'a list -> 'a list
      val init : integer -> (integer -> 'a) -> 'a list
      val map : ('a -> 'b) -> 'a list -> 'b list
      val mapi : (integer -> 'a -> 'b) -> 'a list -> 'b list
      val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
      val fold_right : ('b -> 'a -> 'a) -> 'b list -> 'a -> 'a
      val mem : 'a -> 'a list -> bool
    end

    (** {1 Arrays} *)

    module Array : sig
      val length : 'a array -> integer
      val get : 'a array -> integer -> 'a
      val make : integer -> 'a -> 'a array
      val for_all : ('a -> bool) -> 'a array -> bool
    end
  end

  module Z : sig
    val exists : integer -> integer -> (integer -> bool) -> bool
    (** [exists i j p] is [true] iff the predicate there exists [k] within [i]
        and [j], included, for which [p] holds. *)

    val forall : integer -> integer -> (integer -> bool) -> bool
    (** [forall i j p] is [true] iff the predicate `p` holds forall [k] within
        [i] and [j], included. *)
  end
end

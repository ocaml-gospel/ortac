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

type t = {
  mod_name : string;  (** The name of the module under test *)
  init_sut : string;  (** String representation of the init_sut function *)
  exp_res : expected_result;  (** The expected result of the call *)
  cmd : string;  (** String representation of the call *)
  terms : (string * Ortac_runtime.location) list;
      (** String representation and location of the violated specifications *)
}
(** Information for the bug report in case of test failure *)

type trace = { call : string; res : res }

val report :
  string ->
  string ->
  expected_result ->
  string ->
  (string * Ortac_runtime.location) list ->
  t
(** [report module_name init_sut exp_res cmd terms] *)

val append : t option -> t option -> t option
(** [append a b] appends the violated terms of [a] and [b] if any in the
    returned report *)

val dummy : 'a ty * ('b -> string)
(** A dummy [STM.res] for unknown returned values *)

val is_dummy : res -> bool
val pp_expected_result : expected_result Fmt.t
val pp_terms : (string * Ortac_runtime.location) list Fmt.t
val pp_traces : bool -> expected_result -> trace list Fmt.t

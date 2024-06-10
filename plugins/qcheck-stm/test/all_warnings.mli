(*@ predicate p (x : 'a) = true *)

type 'a t = { v : 'a array }
(*@ mutable model contents : 'a list *)

type s

val make : int -> 'a -> 'a t
(*@ t = make i a
    ensures t.contents = List.init i (fun _ -> a) *)

val constant : unit
(*@ constant
    ensures true *)

val returning_sut : 'a -> 'a t
(*@ t = returning_sut a *)

val multiple_sut_argument : 'a t -> 'a t -> bool
(*@ b = multiple_sut_argument x y *)

val incompatible_type : int t -> bool
(*@ b = incompatible_type t *)

val no_spec : 'a t -> bool
(**)

val ignored_modified : 'a t -> unit
(*@ ignored_modified t
    modifies () *)

val ensures_not_found_for_next_state : 'a t -> unit
(*@ ensures_not_found_for_next_state t
    modifies t.contents *)

val type_not_supported : 'a t -> s
(*@ s = type_not_supported t *)

val functional_argument : ('a -> bool) -> 'a t -> bool
(*@ b = functional_argument p t *)

val ghost_argument : 'a t -> bool
(*@ b = ghost_argument [x : bool] t *)

val ghost_returned_value : 'a t -> bool
(*@ [ x : bool ], b = ghost_returned_value t *)

val unsupported_quantification : 'a t -> bool
(*@ b = unsupported_quantification t
    ensures b = forall a. List.mem a t.contents -> p a *)

val record_not_model_field : 'a t -> bool
(*@ b = record_not_model_field t
    requires Array.length t.v > 0 *)

val return_tuple : 'a t -> 'a * bool
(*@ (a, b) = return_tuple t *)

val term_refer_to_returned_value_next_state : 'a t -> 'a option
(*@ o = term_refer_to_returned_value_next_state t
    modifies t.contents
    ensures t.contents = match o with
                        | None -> old t.contents
                        | Some _ -> old t.contents *)

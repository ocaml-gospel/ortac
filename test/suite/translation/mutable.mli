type t_ephemeral
(*@ ephemeral*)

type 'a with_mutable_model
(*@ mutable model content : 'a set *)

type record_with_mutable_field = { mutable m : int }
type variant_with_known_mutable_field = K of t_ephemeral
type record_with_known_mutable_field = { t : t_ephemeral }
type int_array = int array
type int_ref = int ref
type variant = C of int array
type record = { f : int array }

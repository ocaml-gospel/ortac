type 'a t_dependant = 'a list

val start : unit -> unit

type t_ephemeral
(*@ ephemeral*)

type 'a with_mutable_model
(*@ mutable model content : 'a set *)

type 'a with_deep_mutable_model
(*@ model content : int array *)

type record_with_mutable_flag = { mutable m : int }
type record_with_known_mutable_field = { t : t_ephemeral }
type variant_with_known_mutable_field = K of t_ephemeral
type variant_with_int_array = C0 of int array | C1 of int
type record_with_int_array = { f0 : int array; f1 : int }
type record_with_list_of_known_mutable = { f : t_ephemeral list }
type int_array = int array
type int_ref = int ref
type list_of_mutable = int ref list
type 'a abstract_and_mutable = 'a * int array
type int_ref_dependant_list = int_ref t_dependant

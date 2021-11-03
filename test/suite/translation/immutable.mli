type int_list = int list
type variant = C0 of int | C1 of string * int
type record = { f0 : int; f1 : string }
type abstract_type
type 'a type_with_model
(*@ model content : 'a set *)

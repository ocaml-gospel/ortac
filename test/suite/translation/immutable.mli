type ('a, 'b) t_dependant = 'a * 'b

val start : unit -> unit

type int_list = int list
type variant = C0 of int | C1 of string * int
type record = { f0 : int; f1 : string }
type immutable_t_dependant = (int, char) t_dependant

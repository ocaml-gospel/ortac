type t

val start : unit -> unit

type 'a alpha_list_is_dependant = 'a list
type 'a polymorphic_and_mutable = 'a * int array
type 'a polymorphic_and_immutable = 'a * int
type 'a polymorphic_and_abstract = 'a * t
type ('a, 'b) double_dependant = 'a * 'b

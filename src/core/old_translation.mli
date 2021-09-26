open Ppxlib
open Gospel.Tterm

val process_olds :
  driver:Drv.t ->
  term list ->
  (vsymbol, vsymbol) Hashtbl.t * (expression -> expression)
(** [get_olds] returns a couple [tbl, expr] where [expr] is the OCaml expression
    declaring the copies of the variables used under [old] privimitives, and
    [tbl] is the table associating thee variables to their old counterpart.

    For instance, with the following terms,

    - forall i. i < 0 -> (old a).(i) >= i
    - old (x + 1) = old y

    [expr] contains

    let old_a = copy a in let old_x = copy x in let old_y = copy y in [next]

    and [tbl] contains

    - a -> old_a
    - x -> old_x
    - y -> old_y *)

(*@ function rec aux (c: integer) (n: integer) (i: integer) (k: integer): integer =
    if i > k then c else aux (c * n / i) (n - 1) (i + 1) k *)

(*@ function comb (n: integer) (k: integer): integer =
    aux 1 n 1 k *)

type pascal
(*@ mutable model n: int
    mutable model row: int list *)

val n : pascal -> int [@@model]
val row : pascal -> int list [@@model]
val init : unit -> pascal
(*@ r = init ()
    ensures r.n = 0 *)

val next : pascal -> unit
(*@ next r
    modifies r
    ensures r.n = old r.n + 1
    ensures forall i. 0 <= i <= r.n -> r.row[i] = comb r.n i *)

(* && and || are lazy, but /\ and \/ are not *)

val lazy_bool : int -> int
(*@ y = lazy_bool x
    requires x = x || 1/0 = 2
    requires not (x <> x && 1/0 = 2) *)

val not_lazy_or : int -> int
(*@ y = not_lazy_or x
    requires x = x \/ 1/0 = 2 *)

val not_lazy_and : int -> int
(*@ y = not_lazy_and x
    requires not (x <> x /\ 1/0 = 2) *)

(* variable scope *)

val scope1 : int -> int
(*@ y = scope1 x
    requires let x = true in x
    ensures  let y = true in y *)

(* Terms and formulas *)

val if_forall : int -> int
(*@ y = if_forall x
    requires if forall i. 0 <= i < 10 -> x <> i then x = 10 else x = 3 *)

val equiv : unit -> unit
(*@ equiv ()
    ensures (1 = 2) <-> (2 = 3) *)

val exists_ : unit -> unit
(*@ exists_ ()
    ensures exists x. 0 <= x < 10 /\ x = 3 *)

(* Pattern matching *)

type t = A | B of string

(*@ function t_equal (a: t) (b: t) : bool =
  match a, b with
  | A, A -> true
  | B s0, B s1 -> s0 = s1
  | _, _ -> false *)

val a : t -> unit
(*@ a x
    requires match x with
            | A -> true
            | B s -> false
    requires t_equal x A *)

val b : t -> unit
(*@ b x
    requires match x with
            | A -> false
            | B _ -> true *)

type peano = O | S of peano

(*@ function rec peano_equal (i: peano) (j: peano) : bool =
    match i, j with
    | O, O -> true
    | S i', S j' -> peano_equal i' j'
    | _, _ -> false *)

val succ : peano -> peano
(*@ y = succ x
      ensures peano_equal y (S x) *)

val add : peano -> peano -> peano
(*@ z = add x y
      ensures not (peano_equal x O) -> not (peano_equal z O)
      ensures not (peano_equal y O) -> not (peano_equal z O) *)

val bad_add : peano -> peano -> peano
(*@ z = bad_add x y
      ensures not (peano_equal x O) -> not (peano_equal z O)
      ensures not (peano_equal y O) -> not (peano_equal z  O) *)

type tree = E | N of tree * int * tree

(*@ function rec size (t: tree) : integer =
    match t with E -> 0 | N (l, _, r) -> size l + 1 + size r *)

(*@ function rec tree_equal (t0: tree) (t1: tree) : bool =
    match t0, t1 with
     | E, E -> true
     | N (l0, i0, r0), N (l1, i1, r1) -> i0 = i1 && tree_equal l0 l1 && tree_equal r0 r1
     | _, _ -> false
*)

val size : tree -> int
(*@ s = size t
      pure
      ensures not (tree_equal t E) -> s > 0
      ensures s = size t *)

val size_wrong_spec : tree -> int
(*@ s = size_wrong_spec t
    ensures s <> size t *)

val test_tree : tree -> bool
(*@ b = test_tree t
      ensures b = match t with
                  | E -> true
                  | N (l, x, t) -> tree_equal l t && x = 0 *)

val make_tree : tree -> int -> tree -> tree
(*@ t = make_tree l x r
      ensures tree_equal t (N (l, x, r)) *)

val fill : tree -> int array -> int -> int
(*@ stop = fill t a start
      requires 0 <= start <= Array.length a
      ensures  start <= stop <= Array.length a *)

type alt_tree = Ealt | Nalt of (alt_tree * int * alt_tree)

(*@ function rec alt_tree_equal (t0: alt_tree) (t1: alt_tree) : bool =
  match t0, t1 with
  | Ealt, Ealt -> true
  | Nalt (l0, i0, r0), Nalt (l1, i1, r1) -> i0 = i1 && alt_tree_equal l0 l1 && alt_tree_equal r0 r1
  | _, _ -> false
*)

val make_alt_tree : alt_tree -> int -> alt_tree -> alt_tree
(*@ t = make_alt_tree l x r
      ensures let c = (l, x, r) in alt_tree_equal t (Nalt c) *)

val ref_access : int ref -> int
(*@ y = ref_access x
    ensures y = !x *)

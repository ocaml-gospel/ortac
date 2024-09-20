type 'a t
(*@ mutable model contents : 'a list *)

val make : int -> 'a -> 'a t
(*@ t = make i a
    checks i >= 0
    ensures true /\ true && t.contents = List.init i (fun _ -> a) *)

(*@ function set_contents (c : 'a list) (i : integer) (a : 'a) : 'a list =
          List.mapi (fun j x -> if i = j then a else x) c *)

val set : 'a t -> int -> 'a -> unit
(*@ set t i a
    checks 0 <= i < List.length t.contents
    modifies t
    ensures true /\ true && t.contents = set_contents (old t.contents) i a *)

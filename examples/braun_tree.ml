type 'a t = Empty | Tree of 'a t * 'a * 'a t

let rec get t i =
  match t with
  | Empty -> assert false
  | Tree (l, x, r) ->
      if i = 0 then x
      else if i mod 2 = 1 then get l (i / 2)
      else get r ((i / 2) - 1)

let head = function Empty -> failwith "No head" | Tree (_, x, _) -> x
let left_t = function Empty -> failwith "No left tree" | Tree (l, _, _) -> l
let right_t = function Empty -> assert false | Tree (_, _, r) -> r
let empty () = Empty

let rec diff t n =
  match (t, n) with
  | Empty, 0 -> 0
  | Tree _, 0 -> 1
  | Tree (l, _, _), n when n mod 2 = 1 -> diff l ((n - 1) / 2)
  | Tree (_, _, r), n when n mod 2 = 0 -> diff r ((n - 2) / 2)
  | _, _ -> failwith "No more cases"

let rec size = function
  | Empty -> 0
  | Tree (l, _, r) ->
      let m = size r in
      1 + (2 * m) + diff l m

let rec cons v t =
  match t with
  | Empty -> Tree (Empty, v, Empty)
  | Tree (l, x, r) -> Tree (cons x r, v, l)

let rec tail = function
  | Empty -> failwith "Already empty"
  | Tree (Empty, _, Empty) -> Empty
  | Tree (l, _, r) -> Tree (r, get l 0, tail l)

let rec snoc x t =
  match t with
  | Empty -> Tree (Empty, x, Empty)
  | Tree (l, e, r) ->
      if size t mod 2 = 0 then Tree (l, e, snoc x r) else Tree (snoc x l, e, r)

let rec liat t =
  match t with
  | Empty -> failwith "Already empty"
  | Tree (Empty, _, Empty) -> Empty
  | Tree (l, x, r) ->
      if size t mod 2 = 0 then Tree (liat l, x, r) else Tree (l, x, liat r)

let to_list t =
  let rec aux acc t =
    match t with
    | Empty -> acc
    | Tree (Empty, x, _) -> x :: acc
    | Tree (l, x, Empty) -> aux (x :: acc) l
    | Tree (_, x, _) ->
        let nt = tail t in
        aux (x :: acc) nt
  in
  List.rev (aux [] t)

let of_list l =
  let rec aux acc = function [] -> acc | hd :: tl -> aux (snoc hd acc) tl in
  aux Empty l

let cont = to_list

(* let rec find i t = if t = Empty then failwith "Empty";
  if i = 0 then head t;
  if i mod 2 <> 0
  then find ((i - 1) / 2) (left_t t)
  else find ((i / 2) - 1) (right_t t) *)

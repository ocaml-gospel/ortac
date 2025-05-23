type 'a t = Empty | Tree of 'a t * 'a * 'a t

let empty = Empty

let rec diff t n = match t, n with
  | Empty, 0 -> 0
  | Tree _, 0 -> 1
  | Tree (l, x, r), n when n mod 2 = 1 -> diff l ((n-1)/2)
  | Tree (l, x, r), n when n mod 2 = 0 -> diff r ((n-2)/2)
  | _ , _ -> failwith "No more cases"

let rec size = function
  | Empty -> 0
  | Tree (l, x, r) ->
    let m = size r in
    1 + 2 * m + diff l m

let head = function
  | Empty -> failwith "No head"
  | Tree (_, x, _) -> x

let left_t = function
  | Empty -> failwith "No left tree"
  | Tree (l, _, _) -> l
let right_t = function
  | Empty -> failwith "No right tree"
  | Tree (_, _, r) -> r

let rec add_front x = function
  | Empty -> Tree (Empty, x, Empty)
  | Tree (l, n, r) -> Tree (r, x, add_front n l)

let rec remove_first = function
  | Empty -> failwith "Already empty"
  | Tree (l, n, r) when l = Empty -> n, Empty
  | Tree (l, n, r) ->
    let x, l = remove_first l in
    n, Tree (r, x, l)

let rec append x t = match t with
  | Empty -> Tree (Empty, x, Empty)
  | Tree (l, _, r) ->
    if size t mod 2 = 0
    then append x r
    else append x l

let rec remove_last t = match t with
  | Empty -> failwith "Already empty"
  | Tree (l, n, r) when l = Empty -> n, Empty
  | Tree (l, n, r) ->
    if size t mod 2 = 0
    then remove_last l
    else remove_last r

let rec find i t = if t = Empty then failwith "Empty";
  if i = 0 then head t;
  if i mod 2 <> 0
  then find ((i - 1) / 2) (left_t t)
  else find ((i / 2) - 1) (right_t t)

let cont x t =
  let rec aux acc t = match t with
  | Empty -> acc
  | Tree (Empty, x, _) -> x :: acc
  | Tree (l, x, Empty) -> aux (x :: acc) l
  | Tree (l, x, r) ->
    let _, nt = remove_first t in
    aux (x :: acc) nt
  in
  aux [] t

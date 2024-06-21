open Fmt

type location = { start : Lexing.position; stop : Lexing.position }
type term_kind = Check | Pre | Post | XPost

type error =
  | Violated_axiom
  | Axiom_failure of { exn : exn }
  | Violated_invariant of { term : string; position : term_kind }
  | Violated_condition of { term : string; term_kind : term_kind }
  | Specification_failure of { term : string; term_kind : term_kind; exn : exn }
  | Unexpected_exception of { allowed_exn : string list; exn : exn }
  | Uncaught_checks of { term : string }
  | Unexpected_checks of { terms : string list }

let styled_list l pp = List.fold_left (fun acc x -> styled x acc) pp l
let quoted pp ppf = pf ppf "`%a'" pp

let pp_term_kind =
  using
    (function
      | Check -> "`checks' pre-condition"
      | Pre -> "pre-condition"
      | Post -> "post-condition"
      | XPost -> "exceptional post-condition")
    (styled `Yellow string)

let pp_position =
  using
    (function
      | Pre | Check -> "the pre-state"
      | Post -> "the post-state"
      | XPost -> "an exceptional post-state")
    (styled `Yellow string)

let pp_term = quoted (styled `Bold string)
let pp_terms = list ~sep:(any "@\n") pp_term

let pp_loc =
  let unstyled ppf loc =
    pf ppf "File \"%s\", line %d, characters %d-%d:" loc.start.pos_fname
      loc.start.pos_lnum
      (loc.start.pos_cnum - loc.start.pos_bol)
      (loc.stop.pos_cnum - loc.start.pos_bol)
  in
  styled_list [ `Underline; `Bold ] unstyled

let pp_fun_name = quoted (styled `Blue string)
let pp_quoted_exn = quoted (styled `Bold string)
let pp_exn = using Printexc.to_string pp_quoted_exn
let pp_allowed_exn = list ~sep:comma pp_quoted_exn

let pp_error ppf = function
  | Violated_axiom -> pf ppf "the axiom was %a." (styled `Red string) "violated"
  | Axiom_failure { exn } ->
      pf ppf "the evaluation of the axiom %a:@\n  @[%a@]" (styled `Red string)
        "raised an exception" pp_exn exn
  | Violated_invariant { term; position } ->
      pf ppf "the %a@\n  @[%a@]@\nwas %a in %a." (styled `Yellow string)
        "invariant" pp_term term (styled `Red string) "violated" pp_position
        position
  | Violated_condition { term; term_kind } ->
      pf ppf "the %a@\n  @[%a@]@\nwas %a." pp_term_kind term_kind pp_term term
        (styled `Red string) "violated"
  | Specification_failure { term; term_kind; exn } ->
      pf ppf "the evaluation of the %a@\n  @[%a@]@\n%a:@\n  @[%a@]" pp_term_kind
        term_kind pp_term term (styled `Red string) "raised an exception" pp_exn
        exn
  | Unexpected_exception { allowed_exn; exn } ->
      pf ppf
        "it raised an %a:@\n\
        \  @[%a@]@\n\
         only the following exceptions were declared:@\n\
        \  @[%a@]" (styled `Red string) "unexpected exception" pp_exn exn
        pp_allowed_exn allowed_exn
  | Uncaught_checks { term } ->
      pf ppf
        "a %a in@\n\
        \  @[%a@]@\n\
         was not detected.@\n\
         Function should have raised %a." (styled `Red string)
        "`checks' precondition violation" pp_term term pp_quoted_exn
        "Invalid_argument"
  | Unexpected_checks { terms } ->
      pf ppf
        "it %a@\n\
        \   @[%a@]\n\
         but none of the declared `checks' preconditions@\n\
        \  @[%a@]\n\
         were violated." (styled `Red string) "raised exception" pp_quoted_exn
        "Invalid_argument" pp_terms terms

type error_report = {
  loc : location;
  fun_name : string;
  mutable errors : error list;
}

let pp_error_report ppf { loc; fun_name; errors } =
  let pp_bullet pp ppf = pf ppf "- @[%a@]" pp in
  pf ppf "%a@\n%a in function %a@\n  @[%a@]" pp_loc loc
    (styled_list [ `Bold; `Red ] string)
    "Runtime error" pp_fun_name fun_name
    (list ~sep:(any "@\n") (pp_bullet pp_error))
    errors

exception Error of error_report

module Errors = struct
  type t = error_report

  let create loc fun_name = { loc; fun_name; errors = [] }
  let register t e = t.errors <- e :: t.errors

  let report t =
    match t.errors with
    | [] -> ()
    | _ ->
        pf stderr "%a@." pp_error_report t;
        raise (Error t)
end

exception Partial_function of exn * location

let _ =
  Printexc.register_printer (function
    (* When the exception arises in [next_state] called from [postcond], say, it
       will be wrapped twice *)
    | Partial_function (Partial_function (e, l), _) | Partial_function (e, l) ->
        Some
          (Printf.sprintf
             "Partial function in specification\n\
              %s\n\
              A partial function used in the specification was called out of \
              its definition\n\
              domain, raising the following exception:\n\
              %s"
             (str "%a" pp_loc l) (Printexc.to_string e))
    | _ -> None)

type integer = Z.t

let string_of_integer = Z.to_string

module Aux = struct
  let take n xs =
    let rec aux n acc xs =
      match (n, xs) with
      | _, [] | 0, _ -> Stdlib.List.rev acc
      | _, x :: xs -> aux (n - 1) (x :: acc) xs
    in
    if n < 0 then invalid_arg "take" else aux n [] xs

  let take n xs = take (Z.to_int n) xs

  let rec drop n = function
    | [] -> []
    | xs when n <= 0 -> xs
    | _ :: xs -> drop (n - 1) xs

  let drop n xs = drop (Z.to_int n) xs
end

module Gospelstdlib = struct
  (** Implementation of the Gospel Stdlib

      Note: [bag] and [set] are implemented as decreasing sorted lists where
      each element appears at most once, using the polymorphic comparison.

      For [bag]s, the list contains pairs, where [snd x] is the number of
      occurrences of [fst x], with [snd x > 0].

      Rationale: obviously, the time complexity of these implementations is not
      ideal, but:

      - it is a purely functional implementation (so it can be used with
        QCheck-STM),
      - it is compatible with the polymorphic equality: Ortac uses OCaml's [(=)]
        to implement Gospel's [(=)], this ensures that [set]s and [bag]s can be
        tested for equality in the generated code,
      - they are sorted in decreasing order so that the resulting order
        (according to the polymorphic comparison) on sets and bags is more
        natural. *)

  type 'a sequence = 'a list
  type 'a bag = ('a * Z.t) list
  type 'a set = 'a list

  let succ = Z.succ
  let pred = Z.pred
  let ( ~- ) = Z.( ~- )
  let ( + ) = Z.( + )
  let ( - ) = Z.( - )
  let ( * ) = Z.( * )
  let ( / ) = Z.( / )
  let ( mod ) = Z.( mod )

  let pow x n =
    try Z.pow x (Z.to_int n) with Z.Overflow -> invalid_arg "Exponent too big"

  let abs = Z.abs
  let min = Z.min
  let max = Z.max
  let ( > ) = Z.gt
  let ( >= ) = Z.geq
  let ( < ) = Z.lt
  let ( <= ) = Z.leq
  let logand = Z.logand
  let logor = Z.logor
  let logxor = Z.logxor
  let lognot = Z.lognot
  let shift_left v s = Z.shift_left v (Z.to_int s)
  let shift_right v s = Z.shift_right v (Z.to_int s)
  let shift_right_trunc v s = Z.shift_right_trunc v (Z.to_int s)
  let integer_of_int = Z.of_int
  let max_int = Z.of_int max_int
  let min_int = Z.of_int min_int
  let fst = fst
  let snd = snd
  let ( ~! ) = ( ! )

  module List = struct
    type 'a t = 'a list

    let length l = List.length l |> Z.of_int
    let hd = List.hd
    let tl = List.tl
    let nth l i = List.nth l (Z.to_int i)
    let nth_opt l i = try Some (nth l i) with _ -> None
    let rev = List.rev

    let init i f =
      let i = Z.to_int i in
      let f i = f (Z.of_int i) in
      List.init i f

    let map = List.map

    let mapi f =
      let f i = f (Z.of_int i) in
      List.mapi f

    let fold_left = List.fold_left
    let fold_right = List.fold_right
    let map2 = List.map2
    let for_all = List.for_all
    let _exists = List.exists
    let for_all2 = List.for_all2
    let _exists2 = List.exists2
    let mem = List.mem
    let to_seq = Fun.id
    let of_seq = Fun.id
  end

  module Sequence = struct
    type 'a t = 'a sequence

    let length = List.length
    let empty = []
    let singleton x = [ x ]
    let init = List.init
    let cons x xs = x :: xs
    let snoc xs x = xs @ [ x ]
    let hd = List.hd
    let tl = List.tl
    let append = Stdlib.List.append
    let mem s x = List.mem x s (* is that flip intentional? *)
    let map = List.map
    let filter = Stdlib.List.filter
    let filter_map = Stdlib.List.filter_map
    let get = List.nth

    let set xs n x =
      let err () = failwith "index out of bounds" in
      let n = Z.to_int n in
      let open Stdlib in
      (* to get standard (-) and (<) back *)
      let rec aux = function
        | [], 0 -> [ x ]
        | _ :: xs, 0 -> x :: xs
        | [], _ -> err ()
        | x :: xs, n -> x :: aux (xs, n - 1)
      in
      if n < 0 then err () else aux (xs, n)

    let rev = List.rev
    let fold_left = List.fold_left
    let fold_right = List.fold_right
  end

  let ( ++ ) = Sequence.append
  let __mix_Bub = (* [_] *) Sequence.get
  let __mix_Buddb xs b = (* [_..] *) Aux.drop b xs
  let __mix_Bddub xs e = (* [.._] *) Aux.take (succ e) xs

  let __mix_Buddub xs b e =
    (* [_.._] *)
    if e < b then [] else Aux.take (succ (e - b)) (Aux.drop b xs)

  module BagSet = struct
    module type BagSetType = sig
      type 'a elem
      (** type of the list items in a ['a bag] or ['a set] *)

      val proj : 'a elem -> 'a
      (** [proj x] is the projection of a list item into the actual value *)

      val plusone : 'a -> 'a elem option -> 'a elem
      (** [plusone x y]

          Precondition: [y] is either [Some z] with [proj z = x] or [None] *)

      val minusone : 'a elem -> 'a elem option

      val of_list : 'a list -> 'a elem list
      (** ['a elem list] is really ['a set] or ['a bag] *)
    end

    module Make (T : BagSetType) = struct
      open Stdlib
      (* To recover standard operators, rather than Z ones *)

      (* [focus] and [unfocus] factor out much of the code of all the functions
         doing a search for an element, following the standard principle of
         zippers *)
      let focus x xs =
        let rec aux l = function
          | [] -> (l, None, [])
          | y :: r as r' ->
              let o = compare (T.proj y) x in
              if o > 0 then aux (y :: l) r
              else if o = 0 then (l, Some y, r)
              else (* o < 0 *) (l, None, r')
        in
        aux [] xs

      let unfocus (l, m, r) =
        List.rev_append l (match m with None -> r | Some x -> x :: r)

      let empty = []
      let is_empty = function [] -> true | _ -> false

      let mem x b =
        match focus x b with _, None, _ -> false | _, Some _, _ -> true

      let add x b =
        let l, y, r = focus x b in
        unfocus (l, Some (T.plusone x y), r)

      let singleton x = [ T.plusone x None ]

      let remove x b =
        match focus x b with
        | l, Some y, r -> unfocus (l, T.minusone y, r)
        | _, None, _ -> b

      type side = Left | Right
      type 'a oneortwo = One of 'a * side | Two of 'a * 'a

      let combine f xs ys =
        let cs opt q = match opt with Some v -> v :: q | None -> q in
        let rec aux xs ys =
          match (xs, ys) with
          | [], _ -> List.filter_map (fun y -> f (One (y, Right))) ys
          | _, [] -> List.filter_map (fun x -> f (One (x, Left))) xs
          | x :: xs', y :: ys' ->
              let o = compare (T.proj x) (T.proj y) in
              if o > 0 (* x > y *) then cs (f (One (x, Left))) (aux xs' ys)
              else if o = 0 (* x = y *) then cs (f (Two (x, y))) (aux xs' ys')
              else (* x < y *) cs (f (One (y, Right))) (aux xs ys')
        in
        aux xs ys

      let disjoint xs ys =
        let join = function Two _ -> raise Exit | One _ -> None in
        try
          ignore (combine join xs ys);
          true
        with Exit -> false

      let choose = function [] -> invalid_arg "choose" | x :: _ -> T.proj x
      let choose_opt = function [] -> None | x :: _ -> Some (T.proj x)
      let of_list = T.of_list
      let to_list xs = List.map T.proj xs
      let to_seq = to_list
      let of_seq = of_list
      let map f xs = of_list (List.map (fun x -> f (T.proj x)) xs)
      let fold f b v = List.fold_left (fun v x -> f (T.proj x) v) v b
      let for_all p b = List.for_all (fun x -> p (T.proj x)) b
      let _exists p b = List.exists (fun x -> p (T.proj x)) b
      let filter p b = List.filter (fun x -> p (T.proj x)) b
      let filter_map f b = of_list (List.filter_map (fun x -> f (T.proj x)) b)
      let partition f b = List.partition (fun x -> f (T.proj x)) b
      let compare x y = Z.of_int (compare x y)
    end
  end

  module Bag = struct
    type 'a t = 'a bag

    module BagType = struct
      type 'a elem = 'a * Z.t

      let proj = fst

      let plusone x = function
        | None -> (x, Z.one)
        | Some (y, o) -> (y, Z.succ o)

      let minusone = function
        | _, o when Z.equal o Z.one -> None
        | x, o -> Some (x, Z.pred o)

      let of_list xs =
        let rec rev_group acc x o = function
          | [] -> (x, o) :: acc
          | y :: ys ->
              if x = y then rev_group acc x (Z.succ o) ys
              else rev_group ((x, o) :: acc) y Z.one ys
        in
        match Stdlib.List.fast_sort compare xs with
        | [] -> []
        | x :: xs -> rev_group [] x Z.one xs
    end

    include BagSet.Make (BagType)

    let occurrences x b =
      match focus x b with _, None, _ -> Z.zero | _, Some (_, o), _ -> o

    let union b1 b2 =
      let join = function
        | One (x, _) -> Some x
        | Two ((x, ox), (_, oy)) -> Some (x, Z.max ox oy)
      in
      combine join b1 b2

    let sum b1 b2 =
      let join = function
        | One (x, _) -> Some x
        | Two ((x, ox), (_, oy)) -> Some (x, Z.add ox oy)
      in
      combine join b1 b2

    let inter b1 b2 =
      let join = function
        | One (x, _) -> Some x
        | Two ((x, ox), (_, oy)) -> Some (x, Z.min ox oy)
      in
      combine join b1 b2

    let diff b1 b2 =
      let join = function
        | One (x, Left) -> Some x
        | One (_, Right) -> None
        | Two ((x, xo), (_, yo)) ->
            if Z.gt xo yo then Some (x, Z.sub xo yo) else None
      in
      combine join b1 b2

    let subset b1 b2 =
      let join = function
        | One (_, Left) -> raise Exit
        | Two ((_, xo), (_, yo)) -> if Z.gt xo yo then raise Exit else None
        | _ -> None
      in
      try
        ignore (combine join b1 b2);
        true
      with Exit -> false

    let cardinal b = List.fold_left (fun c (_, o) -> Z.add c o) Z.zero b
  end

  let __mix_Cc = []

  module Set = struct
    type 'a t = 'a set

    module SetType = struct
      type 'a elem = 'a

      let proj = Fun.id
      let plusone x = function None -> x | Some y -> y
      (* We should have x = y here, but we'd rather keep the value
         already in the set *)

      let minusone _ = None

      let of_list xs =
        let rev_compare x y = compare y x in
        Stdlib.List.sort_uniq rev_compare xs
    end

    include BagSet.Make (SetType)

    let union s1 s2 =
      let join = function One (x, _) | Two (x, _) -> Some x in
      combine join s1 s2

    let inter s1 s2 =
      let join = function One _ -> None | Two (x, _) -> Some x in
      combine join s1 s2

    let diff s1 s2 =
      let join = function One (x, Left) -> Some x | _ -> None in
      combine join s1 s2

    let subset s1 s2 =
      let join = function One (_, Left) -> raise Exit | _ -> None in
      try
        ignore (combine join s1 s2);
        true
      with Exit -> false

    let cardinal = List.length
  end

  module Array = struct
    type 'a t = 'a array

    let length arr = Array.length arr |> Z.of_int

    let get arr z =
      if Z.(z < zero || z >= of_int (Array.length arr)) then
        raise (Invalid_argument "Out of array bounds")
      else Array.unsafe_get arr (Z.to_int z)

    let make z =
      if Z.(z > of_int Sys.max_array_length) then
        raise (Invalid_argument "Array length too big")
      else Array.make (Z.to_int z)

    let init n f = Array.init (Z.to_int n) (fun i -> f (Z.of_int i))
    let append = Array.append
    let concat = Array.concat
    let sub xs i j = Array.sub xs (Z.to_int i) (Z.to_int j)
    let map = Array.map
    let mapi f xs = Array.mapi (fun i x -> f (Z.of_int i) x) xs
    let fold_left = Array.fold_left
    let fold_right = Array.fold_right
    let map2 = Array.map2
    let for_all = Array.for_all
    let _exists = Array.exists
    let for_all2 = Array.for_all2
    let _exists2 = Array.exists2
    let mem = Array.mem
    let to_list = Array.to_list
    let of_list = Array.of_list
    let to_seq = Array.to_list
    let of_seq = Array.of_list
    let to_bag a = Bag.of_list (to_list a)
    let permut a1 a2 = to_bag a1 = to_bag a2

    let permut_sub a1 a2 lo hi =
      let open Stdlib in
      let l = Array.length a1 and lo = Z.to_int lo and hi = Z.to_int hi in
      if l <> Array.length a2 || lo < 0 || hi > l || hi < lo then
        invalid_arg "permut_sub";
      try
        for i = 0 to lo - 1 do
          if a1.(i) <> a2.(i) then raise Exit
        done;
        for i = hi to l - 1 do
          if a1.(i) <> a2.(i) then raise Exit
        done;
        permut (Array.sub a1 lo (hi - lo + 1)) (Array.sub a2 lo (hi - lo + 1))
      with Exit -> false
  end

  let __mix_Bmgb m x v y = if y = x then v else m y

  module Map = struct end

  module Order = struct
    let is_pre_order _ =
      failwith "is_pre_order cannot be implemented as a test!"
  end

  module Sys = struct
    let big_endian = Sys.big_endian
    let int_size = Sys.int_size
    let max_array_length = Sys.max_array_length
    let max_string_length = Sys.max_string_length
    let word_size = Sys.word_size
  end
end

module Z = struct
  open Z

  let rec forall start stop p =
    start > stop || (p start && forall (succ start) stop p)

  let rec exists start stop p =
    start <= stop && (p start || exists (succ start) stop p)
end

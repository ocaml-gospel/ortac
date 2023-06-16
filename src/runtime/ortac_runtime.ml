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

type integer = Z.t

module Gospelstdlib = struct
  (** Implementation of the Gospel Stdlib *)

  let niy x = failwith "%s is not implemented yet" x

  type 'a sequence = 'a list

  type 'a bag = unit
  (** dummy placeholder *)

  type 'a set = unit
  (** dummy placeholder *)

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
  let __mix_Bub = Sequence.get
  let __mix_Buddub _ = niy "__mix_Buddub (* [_.._] *)"
  let __mix_Buddb _ = niy "__mix_Buddb (* [_..] *)"
  let __mix_Bddub _ = niy "__mix_Bddub (* [.._] *)"

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
    let to_bag _ = niy "to_bag"
    let permut _ = niy "permut"
    let permut_sub _ = niy "permut_sub"
  end

  module Bag = struct
    type 'a t = ('a bag[@alert "-not_implemented"])

    let occurrences _ = niy "occurrences"
    let empty = ()
    let is_empty _ = niy "is_empty"
    let mem _ = niy "mem"
    let add _ = niy "add"
    let singleton _ = niy "singleton"
    let remove _ = niy "remove"
    let union _ = niy "union"
    let sum _ = niy "sum"
    let inter _ = niy "inter"
    let disjoint _ = niy "disjoint"
    let diff _ = niy "diff"
    let subset _ = niy "subset"
    let choose _ = niy "choose"
    let choose_opt _ = niy "choose_opt"
    let map _ = niy "map"
    let fold _ = niy "fold"
    let for_all _ = niy "for_all"
    let _exists _ = niy "_exists"
    let filter _ = niy "filter"
    let filter_map _ = niy "filter_map"
    let partition _ = niy "partition"
    let cardinal _ = niy "cardinal"
    let to_list _ = niy "to_list"
    let of_list _ = niy "of_list"
    let to_seq _ = niy "to_seq"
    let of_seq _ = niy "of_seq"
  end

  let __mix_Cc = ()

  module Set = struct
    type 'a t = ('a set[@alert "-not_implemented"])

    let compare _ = niy "compare"
    let empty = ()
    let is_empty _ = niy "is_empty"
    let mem _ = niy "mem"
    let add _ = niy "add"
    let singleton _ = niy "singleton"
    let remove _ = niy "remove"
    let union _ = niy "union"
    let inter _ = niy "inter"
    let disjoint _ = niy "disjoint"
    let diff _ = niy "diff"
    let subt _ = niy "subt"
    let cardinal _ = niy "cardinal"
    let choose _ = niy "choose"
    let choose_opt _ = niy "choose_opt"
    let map _ = niy "map"
    let fold _ = niy "fold"
    let for_all _ = niy "for_all"
    let _exists _ = niy "_exists"
    let filter _ = niy "filter"
    let filter_map _ = niy "filter_map"
    let partition _ = niy "partition"
    let to_list _ = niy "to_list"
    let of_list _ = niy "of_list"
    let to_seq _ = niy "to_seq"
    let of_seq _ = niy "of_seq"
  end

  let __mix_Bmgb _ = niy "__mix_Bmgb (* [->] *)"

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

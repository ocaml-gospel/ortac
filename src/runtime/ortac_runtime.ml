open Fmt

type location = { start : Lexing.position; stop : Lexing.position }

type term_kind = Pre | Post | XPost

type error =
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
      | Pre -> "pre-condition"
      | Post -> "post-condition"
      | XPost -> "exceptional post-condition")
    (fun ppf -> pf ppf "%a" (styled `Yellow string))

let pp_term = styled `Bold (quoted string)

let pp_terms = list ~sep:(any "@\n") pp_term

let pp_loc =
  let unstyled ppf loc =
    pf ppf "File \"%s\", line %d, characters %d-%d:" loc.start.pos_fname
      loc.start.pos_lnum
      (loc.start.pos_cnum - loc.start.pos_bol)
      (loc.stop.pos_cnum - loc.start.pos_bol)
  in
  styled_list [ `Underline; `Bold ] unstyled

let pp_fun_name = styled `Blue (quoted string)

let pp_quoted_exn = styled `Bold (quoted string)

let pp_exn = using Printexc.to_string pp_quoted_exn

let pp_allowed_exn = list ~sep:comma pp_quoted_exn

let pp_error ppf = function
  | Violated_condition { term; term_kind } ->
      pf ppf "the %a@\n  @[%a@]@\nwas %a." pp_term_kind term_kind pp_term term
        (styled `Red string)
        "violated"
  | Specification_failure { term; term_kind; exn } ->
      pf ppf
        "the evaluation of the %a@\n  @[%a@]@\nraised an exception:@\n  @[%a@]"
        pp_term_kind term_kind pp_term term pp_exn exn
  | Unexpected_exception { allowed_exn; exn } ->
      pf ppf
        "it raised an unexpected exception:@\n\
        \  @[%a@]@\n\
         only the following exceptions were declared:@\n\
        \  @[%a@]" pp_exn exn pp_allowed_exn allowed_exn
  | Uncaught_checks { term } ->
      pf ppf
        "a `checks' precondition violation in@\n\
        \  @[%a@]@\n\
         was not detected.@\n\
         Function should have raised %a." pp_term term pp_quoted_exn
        "Invalid_argument"
  | Unexpected_checks { terms } ->
      pf ppf
        "it raised exception@\n\
        \   @[%a@]\n\
         but none of the declared `checks' preconditions@\n\
        \  @[%a@]\n\
         were violated." pp_quoted_exn "Invalid_argument" pp_terms terms

type error_report = {
  loc : location;
  fun_name : string;
  mutable errors : error list;
}

let pp_error_report ppf { loc; fun_name; errors } =
  let pp_bullet pp ppf = pf ppf "- @[%a@]" pp in
  pf ppf "%a@\nRuntime error in function %a@\n  @[%a@]" pp_loc loc pp_fun_name
    fun_name
    (list ~sep:(any "@\n") (pp_bullet pp_error))
    errors

exception Error of error_report

let error e = raise (Error e)

module Errors = struct
  type t = error_report

  let create loc fun_name = { loc; fun_name; errors = [] }

  let register t e = t.errors <- e :: t.errors

  let raise = error

  let report = pp_error_report stderr

  let report_and_raise t =
    report t;
    raise t

  let check_and_do f t = match t.errors with [] -> () | _ -> f t
end

module Z = struct
  include Z

  let rec forall start stop p =
    start > stop || (p start && forall (succ start) stop p)

  let rec exists start stop p =
    start <= stop && (p start || exists (succ start) stop p)
end

module Array = struct
  let create z =
    if Z.(z > of_int Sys.max_array_length) then
      raise (Invalid_argument "Array length too big")
    else Array.make (Z.to_int z)

  let get arr z =
    if Z.(z < zero || z >= of_int (Array.length arr)) then
      raise (Invalid_argument "Out of array bounds")
    else Array.unsafe_get arr (Z.to_int z)

  let length arr = Array.length arr |> Z.of_int
end

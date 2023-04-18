open Monolith
include Ortac_runtime

module Errors = struct
  type t = error_report

  let create loc fun_name = { loc; fun_name; errors = [] }
  let register t e = t.errors <- e :: t.errors

  let is_pre = function
    | Violated_condition e -> e.term_kind = Pre
    | Specification_failure e -> e.term_kind = Pre
    | Violated_invariant e -> e.position = Pre
    | _ -> false

  let report t =
    match t.errors with
    | [] -> ()
    | errs when List.exists is_pre errs -> raise Monolith.PleaseBackOff
    | _ ->
        Fmt.flush Fmt.stderr (pp_error_report Fmt.stderr t);
        (* pp_error_report Fmt.stderr t; *)
        raise (Error t)
end

let print_record = PPrint.OCaml.record
let print_variant = PPrint.OCaml.variant
let print_tuple = PPrint.OCaml.tuple
let constructible_int = int_within (Gen.semi_open_interval (-420000) 420000)
let int = ifpol constructible_int int
let positive_int = int_within (Gen.int Int.max_int)

let array spec =
  map_outof Stdlib.Array.of_list
    (Stdlib.Array.of_list, constant "array from list")
    (list spec)

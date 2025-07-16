open STM

type expected_result =
  | Value of res
  | Protected_value of res
  | Exception of string
  | Out_of_domain

type t = {
  mod_name : string;
  init_sut : string;
  exp_res : expected_result;
  cmd : string;
  terms : (string * Ortac_runtime.location) list;
}

type trace = { call : string; res : res }

let report mod_name init_sut exp_res cmd terms =
  { mod_name; init_sut; exp_res; cmd; terms }

let append a b =
  match (a, b) with
  | None, None -> None
  | Some _, None -> a
  | None, Some _ -> b
  | Some r0, Some r1 ->
      assert (r0.cmd = r1.cmd);
      Some { r0 with terms = r0.terms @ r1.terms }

type _ ty += Dummy : _ ty

let dummy = (Dummy, fun _ -> Printf.sprintf "unknown value")
let is_dummy = function Res ((Dummy, _), _) -> true | _ -> false

open Fmt

let pp_expected_result ppf = function
  | Value res when not @@ is_dummy res ->
      pf ppf "assert (r = %s)@\n" (show_res res)
  | Protected_value res when not @@ is_dummy res ->
      pf ppf "assert (r = Ok %s)@\n" (show_res res)
  | Exception exn ->
      pf ppf
        "assert (@[match r with@\n\
        \  @[| Error (%s _) -> true@\n\
         | _ -> false@]@])@\n"
        exn
  | Out_of_domain ->
      pf ppf
        "(* @[Partial function called out of domain@\n\
         in the computation of the expected value.@] *)@\n"
  | _ -> ()

let pp_terms ppf err =
  let open Fmt in
  let pp_aux ppf (term, l) =
    pf ppf "@[%a@\n  @[%s@]@]@\n" Ortac_runtime.pp_loc l term
  in
  pf ppf "%a" (list ~sep:(any "@\n") pp_aux) err

let pp_traces assert_flag exp_res =
  let rec aux ppf = function
    | [ { call; res } ] when assert_flag ->
        pf ppf "%s@\n%a(* returned %s *)@\n" call pp_expected_result exp_res
          (show_res res)
    | { call; res } :: xs ->
        pf ppf "%s@\n(* returned %s *)@\n" call (show_res res);
        aux ppf xs
    | _ -> ()
  in
  aux

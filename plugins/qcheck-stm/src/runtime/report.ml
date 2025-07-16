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

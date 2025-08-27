open STM

type expected_result =
  | Value of res
  | Protected_value of res
  | Exception of string
  | Out_of_domain

type t = {
  mod_name : string;
  init_sut : string;
  ret : expected_result;
  cmd : string;
  terms : (string * Ortac_runtime.location) list;
}

let report mod_name init_sut ret cmd terms =
  { mod_name; init_sut; ret; cmd; terms }

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

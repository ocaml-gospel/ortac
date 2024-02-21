open STM
include Ortac_runtime

type report = {
  mod_name : string;
  ret : res option;
  cmd : string;
  terms : (string * location) list;
}

let report mod_name ret cmd terms = { mod_name; ret; cmd; terms }

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

module Make (Spec : Spec) = struct
  open QCheck
  module Internal = Internal.Make (Spec) [@alert "-internal"]

  let pp_trace ppf (trace, mod_name, ret) =
    let open Fmt in
    let pp_expected ppf = function
      | Some ret when not @@ is_dummy ret ->
          pf ppf "assert (r = %s)@\n" (show_res ret)
      | _ -> ()
    in
    let rec aux ppf = function
      | [ (c, r) ] ->
          pf ppf "let r = %s@\n%a(* returned %s *)@\n" (Spec.show_cmd c)
            pp_expected ret (show_res r)
      | (c, r) :: xs ->
          pf ppf "let _ = %s@\n(* returned %s *)@\n" (Spec.show_cmd c)
            (show_res r);
          aux ppf xs
      | _ -> assert false
    in
    pf ppf "@[open %s@\nlet sut = init_sut@\n%a@]" mod_name aux trace

  let pp_terms ppf err =
    let open Fmt in
    let pp_aux ppf (term, l) = pf ppf "@[%a@\n  @[%s@]@]@\n" pp_loc l term in
    pf ppf "%a" (list ~sep:(any "@\n") pp_aux) err

  let message trace report =
    Test.fail_reportf
      "Gospel specification violation in function %s\n\
       @;\
      \  @[%a@]@\n\
       when executing the following sequence of operations:@\n\
       @;\
      \  @[%a@]@." report.cmd pp_terms report.terms pp_trace
      (trace, report.mod_name, report.ret)

  let rec check_disagree postcond s sut cs =
    match cs with
    | [] -> None
    | c :: cs -> (
        let res = Spec.run c sut in
        (* This functor will be called after a modified postcond has been
           defined, returning a list of 3-plets containing the command, the
           term and the location *)
        match postcond c s res with
        | None -> (
            let s' = Spec.next_state c s in
            match check_disagree postcond s' sut cs with
            | None -> None
            | Some (rest, report) -> Some ((c, res) :: rest, report))
        | Some report -> Some ([ (c, res) ], report))

  let agree_prop check_init_state postcond cs =
    check_init_state ();
    assume (Internal.cmds_ok Spec.init_state cs);
    let sut = Spec.init_sut () in
    (* reset system's state *)
    let res =
      try Ok (check_disagree postcond Spec.init_state sut cs)
      with exn -> Error exn
    in
    let () = Spec.cleanup sut in
    let res = match res with Ok res -> res | Error exn -> raise exn in
    match res with None -> true | Some (trace, report) -> message trace report

  let agree_test ~count ~name wrapped_init_state postcond =
    Test.make ~name ~count
      (Internal.arb_cmds Spec.init_state)
      (agree_prop wrapped_init_state postcond)
end

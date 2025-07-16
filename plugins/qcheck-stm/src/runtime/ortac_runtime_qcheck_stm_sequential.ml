open Ortac_runtime_qcheck_stm_util
open STM
include Ortac_runtime
module Report = Report
module Model = Stores.Model
module SUT = Stores.SUT

module Make (Spec : Spec) = struct
  open Report
  open QCheck
  module Internal = Internal.Make (Spec) [@alert "-internal"]

  let pp_program max_suts ppf (trace, mod_name, init_sut, exp_res) =
    let open Fmt in
    let inits =
      List.init max_suts (fun i -> Format.asprintf "let sut%d = %s" i init_sut)
    in
    pf ppf
      "@[%s@\n\
       open %s@\n\
       let protect f = try Ok (f ()) with e -> Error e@\n\
       %a@\n\
       %a@]"
      "[@@@ocaml.warning \"-8\"]" mod_name
      Format.(
        pp_print_list ~pp_sep:(fun pf _ -> fprintf pf "@\n") pp_print_string)
      inits (pp_traces true exp_res) trace

  let message max_suts trace report =
    Test.fail_reportf
      "Gospel specification violation in function %s\n\
       @;\
      \  @[%a@]@\n\
       when executing the following sequence of operations:@\n\
       @;\
      \  @[%a@]@."
      report.cmd pp_terms report.terms (pp_program max_suts)
      (trace, report.mod_name, report.init_sut, report.exp_res)

  let rec check_disagree postcond ortac_show_cmd s sut cs =
    match cs with
    | [] -> None
    | c :: cs -> (
        let res = Spec.run c sut in
        let call = ortac_show_cmd c sut (cs = []) res in
        (* This functor will be called after a modified postcond has been
           defined, returning a list of 3-plets containing the command, the
           term and the location *)
        match postcond c s res with
        | None -> (
            let s' = Spec.next_state c s in
            match check_disagree postcond ortac_show_cmd s' sut cs with
            | None -> None
            | Some (rest, report) -> Some ({ call; res } :: rest, report))
        | Some report -> Some ([ { call; res } ], report))

  let agree_prop max_suts check_init_state ortac_show_cmd postcond cs =
    check_init_state ();
    assume (Internal.cmds_ok Spec.init_state cs);
    let sut = Spec.init_sut () in
    (* reset system's state *)
    let res =
      try Ok (check_disagree postcond ortac_show_cmd Spec.init_state sut cs)
      with exn -> Error exn
    in
    let () = Spec.cleanup sut in
    let res = match res with Ok res -> res | Error exn -> raise exn in
    match res with
    | None -> true
    | Some (trace, report) -> message max_suts trace report

  let agree_test ~count ~name max_suts wrapped_init_state ortac_show_cmd
      postcond =
    let test_prop =
      agree_prop max_suts wrapped_init_state ortac_show_cmd postcond
    in
    let env_var_fail () =
      let msg = "ORTAC_QCHECK_STM_TIMEOUT must be set to a positive integer" in
      Printf.eprintf "%s" msg;
      exit 1
    in
    let wrapped_prop =
      match Sys.getenv_opt "ORTAC_QCHECK_STM_TIMEOUT" with
      | None -> test_prop
      | Some time ->
          let timeout = try int_of_string time with _ -> env_var_fail () in
          if timeout <= 0 then env_var_fail ();
          Util.fork_prop_with_timeout timeout test_prop
    in
    Test.make ~name ~count (Internal.arb_cmds Spec.init_state) wrapped_prop
end

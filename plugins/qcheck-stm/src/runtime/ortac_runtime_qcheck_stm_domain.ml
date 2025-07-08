open Ortac_runtime_qcheck_stm_util
open STM
include Ortac_runtime
module Report = Report
module Model = Stores.Model
module SUT = Stores.SUT

(* This is a modified version of STM_domain.MakeExt *)
module MakeExt (Spec : SpecExt) = struct
  open Util
  open QCheck
  open Report
  open Internal.Make (Spec) [@alert "-internal"]

  type pos = Prefix | Dom1 | Dom2

  type traces = {
    where_it_failed : pos;
    trace_prefix : trace list;
    trace_dom1 : trace list;
    trace_dom2 : trace list;
  }

  let empty where_it_failed =
    { where_it_failed; trace_prefix = []; trace_dom1 = []; trace_dom2 = [] }

  let start_traces pos call res =
    let traces = empty pos in
    match pos with
    | Prefix -> { traces with trace_prefix = [ { call; res } ] }
    | Dom1 -> { traces with trace_dom1 = [ { call; res } ] }
    | Dom2 -> { traces with trace_dom2 = [ { call; res } ] }

  let add_trace pos call res traces =
    match pos with
    | Prefix ->
        { traces with trace_prefix = { call; res } :: traces.trace_prefix }
    | Dom1 -> { traces with trace_dom1 = { call; res } :: traces.trace_dom1 }
    | Dom2 -> { traces with trace_dom2 = { call; res } :: traces.trace_dom2 }

  (** [check_obs postcond ortac_show_cmd sut pref cs1 cs2 state]

      Returns [None] iff there is an interleaving of [cs1] and [cs2], after the
      sequential evaluation of [pref], that is coherent with the observed
      behaviour.

      Returns [Some (traces, report)] iff there are no interleaving explaining
      the observed behaviour (meaning the tool succeed at finding a bug)*)
  let check_obs postcond ortac_show_cmd sut =
    let build_and_add_trace pos cmd res (traces, report) =
      let call = ortac_show_cmd cmd sut false res in
      let traces = add_trace pos call res traces in
      (traces, report)
    in
    (* [sequential] handles [cmd] when the run is sequential (sequential
       prefix or one of the spawned domains when the orther one is done. It
       then decide what to do next. *)
    let rec sequential pos cmd res pref cs1 cs2 s =
      let none =
        (* if the postconditions of the command are respected, proceed
           and add the traces iff some postconditions in the suffix are
           violated *)
        let f = build_and_add_trace pos cmd res in
        let s' = Spec.next_state cmd s in
        Option.map f @@ aux pref cs1 cs2 s'
      and some report =
        (* if the postconditions of the command are violated, start
           tracing and send back the report *)
        let call = ortac_show_cmd cmd sut true res in
        let traces = start_traces pos call res in
        Some (traces, report)
      in
      Option.fold ~none ~some @@ postcond cmd s res
    and aux pref cs1 cs2 s =
      match pref with
      | (c, res) :: pref' -> sequential Prefix c res pref' cs1 cs2 s
      | [] as pref -> (
          match (cs1, cs2) with
          | [], [] -> None
          | ([] as cs1), (c2, res2) :: cs2' ->
              sequential Dom2 c2 res2 pref cs1 cs2' s
          | (c1, res1) :: cs1', ([] as cs2) ->
              sequential Dom1 c1 res1 pref cs1' cs2 s
          | (c1, res1) :: cs1', (c2, res2) :: cs2' -> (
              (* First, let's defined what to do if all the interleaving
                 starting with c1 fail *)
              let proceed_with_cs2 _ =
                let proceed () =
                  (* postconditions of c2 is respected, proceed *)
                  aux pref cs1 cs2' @@ Spec.next_state c2 s
                and found_bug report =
                  (* postconditions of c2 are not respected, start traces as
                     cs1 as already been explored. *)
                  let call = ortac_show_cmd c2 sut true res2
                  and expected_res = Spec.run c2 sut in
                  let traces = start_traces Dom2 call expected_res in
                  Some (traces, report)
                in
                match postcond c2 s res2 with
                | None ->
                    let f = build_and_add_trace Dom2 c2 res2 in
                    Option.map f @@ proceed ()
                | Some report -> found_bug report
              in
              (* Check interleaving starting with c1 *)
              match postcond c1 s res1 with
              | None -> (
                  (* postconditions of c1 are respected, proceed *)
                  let s' = Spec.next_state c1 s in
                  (* explore all the interleavings once c1 is evaluated *)
                  match aux pref cs1' cs2 s' with
                  | None ->
                      (* There is an interleaving explaining the observed
                         behaviour *)
                      None
                  | Some _ ->
                      (* No interleaving explain the observed behaviour after
                         the evaluation of c1 *)
                      proceed_with_cs2 ())
              | Some _ ->
                  (* No interleaving starting with c1 explain the observed
                     behaviour *)
                  proceed_with_cs2 ()))
    in
    aux

  let all_interleavings_ok (seq_pref, cmds1, cmds2) =
    all_interleavings_ok seq_pref cmds1 cmds2 Spec.init_state

  let arb_cmds_triple = arb_cmds_triple
  let arb_triple = arb_triple

  (* Common magic constants, taken from original implementation of STM_domain *)
  let rep_count = 25 (* No. of repetitions of the non-deterministic property *)
  let retries = 10 (* Additional factor of repetition during shrinking *)
  let seq_len = 20 (* max length of the sequential prefix *)
  let par_len = 12 (* max length of the parallel cmd lists *)

  (* Shamelessly copy-pasted from the original STM_domains *)
  (* operate over arrays to avoid needless allocation underway *)
  let interp_sut_res sut cs =
    let cs_arr = Array.of_list cs in
    let res_arr =
      Array.map
        (fun c ->
          Domain.cpu_relax ();
          Spec.run c sut)
        cs_arr
    in
    List.combine cs (Array.to_list res_arr)

  (* Shamelessly copy-pasted from the original STM_domains *)
  let run_par sut seq_pref cmds1 cmds2 =
    let pref_obs = Spec.wrap_cmd_seq @@ fun () -> interp_sut_res sut seq_pref in
    let barrier = Atomic.make 2 in
    let main cmds () =
      Spec.wrap_cmd_seq @@ fun () ->
      Atomic.decr barrier;
      while Atomic.get barrier <> 0 do
        Domain.cpu_relax ()
      done;
      try Ok (interp_sut_res sut cmds) with exn -> Error exn
    in
    let dom1 = Domain.spawn (main cmds1) in
    let dom2 = Domain.spawn (main cmds2) in
    let obs1 = Domain.join dom1 in
    let obs2 = Domain.join dom2 in
    let () = Spec.cleanup sut in
    let obs1 = match obs1 with Ok v -> v | Error exn -> raise exn in
    let obs2 = match obs2 with Ok v -> v | Error exn -> raise exn in
    (pref_obs, obs1, obs2)

  let agree_prop _max_suts wrapped_init_state ortac_show_cmd postcond
      (seq_pref, cmds1, cmds2) =
    wrapped_init_state ();
    let sut = Spec.init_sut () in
    let pref_obs, obs1, obs2 = run_par sut seq_pref cmds1 cmds2 in
    let res =
      check_obs postcond ortac_show_cmd sut pref_obs obs1 obs2 Spec.init_state
    in
    match res with
    | None -> true
    | Some _ ->
        Test.fail_reportf "  Results incompatible with linearized model\n\n%s"
        @@ print_triple_vertical ~fig_indent:5 ~res_width:35
             (fun (c, r) ->
               Printf.sprintf "%s : %s" (Spec.show_cmd c) (show_res r))
             (pref_obs, obs1, obs2)

  let agree_test ~count ~name max_suts wrapped_init_state ortac_show_cmd
      postcond =
    let max_gen = 3 * count in
    (* precond filtering may require extra generation: max. 3*count though *)
    let test_prop =
      agree_prop max_suts wrapped_init_state ortac_show_cmd postcond
    in
    Test.make ~retries ~max_gen ~count ~name (arb_cmds_triple seq_len par_len)
      (fun triple ->
        assume (all_interleavings_ok triple);
        repeat rep_count test_prop triple)
  (* 25 times each, then 25 * 10 times when shrinking *)
end

module Make (Spec : Spec) = MakeExt (struct
  include SpecDefaults
  include Spec
end)

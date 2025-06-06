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
  open Internal.Make (Spec) [@alert "-internal"]

  let check_obs postcond =
    (* ignore the report for now *)
    let postcond c s r = Option.is_none @@ postcond c s r in
    let rec aux pref cs1 cs2 s =
      match pref with
      | (c, res) :: pref' ->
          let b = postcond c s res in
          b && aux pref' cs1 cs2 (Spec.next_state c s)
      | [] -> (
          match (cs1, cs2) with
          | [], [] -> true
          | [], (c2, res2) :: cs2' ->
              let b = postcond c2 s res2 in
              b && aux pref cs1 cs2' (Spec.next_state c2 s)
          | (c1, res1) :: cs1', [] ->
              let b = postcond c1 s res1 in
              b && aux pref cs1' cs2 (Spec.next_state c1 s)
          | (c1, res1) :: cs1', (c2, res2) :: cs2' ->
              (let b1 = postcond c1 s res1 in
               b1 && aux pref cs1' cs2 (Spec.next_state c1 s))
              ||
              let b2 = postcond c2 s res2 in
              b2 && aux pref cs1 cs2' (Spec.next_state c2 s))
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
  let run_par seq_pref cmds1 cmds2 =
    let sut = Spec.init_sut () in
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

  let agree_prop _max_suts wrapped_init_state _ortac_show_cmd postcond
      (seq_pref, cmds1, cmds2) =
    wrapped_init_state ();
    let pref_obs, obs1, obs2 = run_par seq_pref cmds1 cmds2 in
    check_obs postcond pref_obs obs1 obs2 Spec.init_state
    || Test.fail_reportf "  Results incompatible with linearized model\n\n%s"
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

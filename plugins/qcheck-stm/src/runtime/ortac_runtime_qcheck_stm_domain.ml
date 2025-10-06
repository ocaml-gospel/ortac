open Ortac_runtime_qcheck_stm_util
open STM
include Ortac_runtime
module Report = Report
module Model = Stores.Model
module SUT = Stores.SUT

module type SpecExtOrtac = sig
  include SpecExt

  val arb_cmd_seq : state -> cmd QCheck.arbitrary
  val arb_cmd_dom0 : state -> cmd QCheck.arbitrary
  val arb_cmd_dom1 : state -> cmd QCheck.arbitrary
end

module type SpecOrtac = sig
  include Spec

  val arb_cmd_seq : state -> cmd QCheck.arbitrary
  val arb_cmd_dom0 : state -> cmd QCheck.arbitrary
  val arb_cmd_dom1 : state -> cmd QCheck.arbitrary
end

(* This is a modified version of STM_domain.MakeExt *)
module MakeExt (Spec : SpecExtOrtac) = struct
  open Util
  open QCheck
  open Report
  open Internal.Make (Spec) [@alert "-internal"]

  type pos = Prefix | Tail1 | Tail2

  type traces = {
    where_it_failed : pos;
    trace_prefix : trace list;
    trace_tail_1 : trace list;
    trace_tail_2 : trace list;
  }

  let empty where_it_failed =
    { where_it_failed; trace_prefix = []; trace_tail_1 = []; trace_tail_2 = [] }

  let start_traces pos call res =
    let traces = empty pos in
    match pos with
    | Prefix -> { traces with trace_prefix = [ { call; res } ] }
    | Tail1 -> { traces with trace_tail_1 = [ { call; res } ] }
    | Tail2 -> { traces with trace_tail_2 = [ { call; res } ] }

  let add_trace (pos, trace) traces =
    match pos with
    | Prefix -> { traces with trace_prefix = trace :: traces.trace_prefix }
    | Tail1 -> { traces with trace_tail_1 = trace :: traces.trace_tail_1 }
    | Tail2 -> { traces with trace_tail_2 = trace :: traces.trace_tail_2 }

  let append_traces traces (pos, xs) =
    match pos with
    | Prefix -> { traces with trace_prefix = traces.trace_prefix @ xs }
    | Tail1 -> { traces with trace_tail_1 = traces.trace_tail_1 @ xs }
    | Tail2 -> { traces with trace_tail_2 = traces.trace_tail_2 @ xs }

  let get_traces pos traces =
    match pos with
    | Prefix -> traces.trace_prefix
    | Tail1 -> traces.trace_tail_1
    | Tail2 -> traces.trace_tail_2

  let ( <+> ) marked_trace =
    Option.map (fun (traces, report) -> (add_trace marked_trace traces, report))

  let ( <++> ) o marked_traces =
    Option.map
      (fun (traces, report) -> (append_traces traces marked_traces, report))
      o

  let ( &&& ) o1 o2 = match o1 with None -> Lazy.force o2 | _ -> o1
  let ( ||| ) o1 o2 = match o1 with None -> None | Some _ -> Lazy.force o2

  let check_obs ortac_show_cmd postcond =
    let postcond pos cmd state res =
      let f report =
        let call = ortac_show_cmd cmd (Spec.next_state cmd state) true res in
        (start_traces pos call res, report)
      in
      Option.map f @@ postcond cmd state res
    in
    let mk_trace pos last cmd state res =
      let call = ortac_show_cmd cmd state last res in
      (pos, { call; res })
    in
    let trace_suffix pos state cs =
      let rec aux state = function
        | [] -> []
        | (cmd, res) :: tail ->
            let state' = Spec.next_state cmd state in
            let call = ortac_show_cmd cmd state' (tail = []) res in
            { call; res } :: aux state' tail
      in
      (pos, aux state cs)
    in
    let rec aux pref cs1 cs2 s =
      match pref with
      | (c, res) :: pref' ->
          postcond Prefix c s res
          &&& lazy
                (let s' = Spec.next_state c s in
                 mk_trace Prefix (pref' = []) c s' res <+> aux pref' cs1 cs2 s')
      | [] -> (
          match (cs1, cs2) with
          | [], [] -> None
          | [], (c2, res2) :: cs2' ->
              postcond Tail2 c2 s res2
              &&& lazy
                    (let s' = Spec.next_state c2 s in
                     mk_trace Tail2 (cs2' = []) c2 s' res2
                     <+> aux pref cs1 cs2' s')
          | (c1, res1) :: cs1', [] ->
              postcond Tail1 c1 s res1
              &&& lazy
                    (let s' = Spec.next_state c1 s in
                     mk_trace Tail1 (cs1' = []) c1 s' res1
                     <+> aux pref cs1' cs2 s')
          | (c1, res1) :: cs1', (c2, res2) :: cs2' ->
              postcond Tail1 c1 s res1
              <++> trace_suffix Tail2 s cs2
              &&& lazy
                    (let s' = Spec.next_state c1 s in
                     mk_trace Tail1 (cs1' = []) c1 s' res1
                     <+> aux pref cs1' cs2 s')
              ||| lazy
                    (postcond Tail2 c2 s res2
                    <++> trace_suffix Tail1 s cs1
                    &&& lazy
                          (let s' = Spec.next_state c2 s in
                           mk_trace Tail2 (cs2' = []) c2 s' res2
                           <+> aux pref cs1 cs2' (Spec.next_state c2 s))))
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

  let pp_prefix exp_res ppf traces =
    let assert_flag = traces.where_it_failed = Prefix in
    pp_traces assert_flag exp_res ppf @@ get_traces Prefix traces

  let pp_spawned pos ppf traces =
    let open Fmt in
    let rec aux ppf = function
      | [ { call; res } ] ->
          pf ppf "%s in@\n(* returned %s *)@\n r" call (show_res res)
      | { call; res } :: xs ->
          pf ppf "%s in@\n(* returned %s *)@\n" call (show_res res);
          aux ppf xs
      | _ -> ()
    in
    match get_traces pos traces with [] -> pf ppf " ()" | xs -> aux ppf xs

  let pp_program max_suts ppf (traces, report) =
    let open Fmt in
    let inits =
      List.init max_suts (fun i ->
          Format.asprintf "let sut%d = %s" i report.init_sut)
    in
    let join1 =
      match traces.where_it_failed with
      | Tail1 -> "let r = Domain.join dom1"
      | _ -> "let _ = Domain.join dom1"
    and join2 =
      match traces.where_it_failed with
      | Tail2 -> "let r = Domain.join dom2"
      | _ -> "let _ = Domain.join dom2"
    in
    pf ppf
      "@[%s@\n\
       open %s@\n\
       let protect f = try Ok (f ()) with e -> Error e@\n\
       %a@\n\
       %a@\n\
       let main1 () =@\n\
      \  @[%a@]@\n\
       let main2 () =@\n\
      \  @[%a@]@\n\
       @\n\
       let dom1 = Domain.spawn main1@\n\
       let dom2 = Domain.spawn main2@\n\
       %s@\n\
       %s@\n\
       %a@\n"
      "[@@@ocaml.warning \"-8\"]" report.mod_name
      Format.(
        pp_print_list ~pp_sep:(fun pf _ -> fprintf pf "@\n") pp_print_string)
      inits (pp_prefix report.exp_res) traces (pp_spawned Tail1) traces
      (pp_spawned Tail2) traces join1 join2 pp_expected_result report.exp_res

  let agree_prop max_suts wrapped_init_state ortac_show_cmd postcond
      (seq_pref, cmds1, cmds2) =
    wrapped_init_state ();
    let pref_obs, obs1, obs2 = run_par seq_pref cmds1 cmds2 in
    match
      check_obs ortac_show_cmd postcond pref_obs obs1 obs2 Spec.init_state
    with
    | None -> true
    | Some (traces, report) ->
        Report.message (pp_program max_suts) traces report

  let agree_test ~count ~name max_suts wrapped_init_state ortac_show_cmd
      postcond =
    let max_gen = 3 * count in
    (* precond filtering may require extra generation: max. 3*count though *)
    let test_prop =
      agree_prop max_suts wrapped_init_state ortac_show_cmd postcond
    in
    Test.make ~retries ~max_gen ~count ~name
      (arb_triple seq_len par_len Spec.arb_cmd_seq Spec.arb_cmd_dom0
         Spec.arb_cmd_dom1) (fun triple ->
        assume (all_interleavings_ok triple);
        repeat rep_count test_prop triple)
  (* 25 times each, then 25 * 10 times when shrinking *)
end

module Make (Spec : SpecOrtac) = MakeExt (struct
  include SpecDefaults
  include Spec
end)

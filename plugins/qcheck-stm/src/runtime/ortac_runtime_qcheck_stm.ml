open STM
include Ortac_runtime

type expected_result =
  | Value of res
  | Protected_value of res
  | Exception of string

type report = {
  mod_name : string;
  init_sut : string;
  ret : expected_result;
  cmd : string;
  terms : (string * location) list;
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

module Model = struct
  module Make (M : sig
    type elt

    val init : elt
  end) : sig
    type elt
    (** The type of a model of a single state *)

    type t
    (** A value of type [t] represents an immutable model of a stack of states *)

    val create : int -> unit -> t
    (** [create n ()] creates a model with [n] initial elements *)

    val size : t -> int
    (** [size t] returns the number of elements in the model [t] *)

    val drop_n : t -> int -> t
    (** [drop_n t n] returns a new model with the [n] uppermost elements removed *)

    val push : t -> elt -> t
    (** [push t e] returns a new model with [e] as the topmost element *)

    val get : t -> int -> elt
    (** [get t n] returns the [n]th element of the model [t] *)
  end
  with type elt := M.elt = struct
    type elt = M.elt
    type t = elt list

    let create n () : t = List.init n (fun _ -> M.init)
    let size = List.length
    let rec drop_n t n = if n = 0 then t else drop_n (List.tl t) (n - 1)
    let push t e = e :: t

    let get t n =
      try List.nth t n with _ -> failwith ("nth: " ^ string_of_int n)
  end
end

module SUT = struct
  module Make (M : sig
    type sut

    val init : unit -> sut
  end) : sig
    type elt
    (** Type of a single SUT *)

    type t
    (** Values of type [t] represent a stack of SUTs *)

    val create : int -> unit -> t
    (** [create n ()] creates an initial stack with [n] SUTs *)

    val clear : t -> unit
    (** [clear t] removes all elements from the stack [t] of SUTs *)

    val size : t -> int
    (** [size t] returns the number of SUTs currently on the stack [t] *)

    val pop : t -> elt
    (** [pop t] pops and returns the topmost element of the stack [t] of SUTs *)

    val push : t -> elt -> unit
    (** [push t e] pushes the SUT e onto the stack [t] of SUTs *)

    val get_name : t -> int -> string
    (** [get_name t n] returns the name for the [n]th element of the stack [t]
        of SUTs *)
  end
  with type elt = M.sut = struct
    type elt = M.sut
    type t = elt Stack.t

    let create n () : t =
      let t = Stack.create () in
      for _ = 0 to n - 1 do
        Stack.push (M.init ()) t
      done;
      t

    let clear (t : t) = Stack.clear t
    let size (t : t) = Stack.length t
    let pop (t : t) = Stack.pop t
    let push (t : t) (e : elt) = Stack.push e t
    let get_name (t : t) n = Format.asprintf "sut%d" (Stack.length t - n - 1)
  end
end

module Make (Spec : Spec) = struct
  open QCheck
  module Internal = Internal.Make (Spec) [@alert "-internal"]

  let pp_trace max_suts ppf (trace, mod_name, init_sut, ret) =
    let open Fmt in
    let pp_expected ppf = function
      | Value ret when not @@ is_dummy ret ->
          pf ppf "assert (r = %s)@\n" (show_res ret)
      | Protected_value ret when not @@ is_dummy ret ->
          pf ppf "assert (r = Ok %s)@\n" (show_res ret)
      | Exception exn ->
          pf ppf
            "assert (@[match r with@\n\
            \  @[| Error (%s _) -> true@\n\
             | _ -> false@]@])@\n"
            exn
      | _ -> ()
    in
    let rec aux ppf = function
      | [ (c, r) ] ->
          pf ppf "%s@\n%a(* returned %s *)@\n" c pp_expected ret (show_res r)
      | (c, r) :: xs ->
          pf ppf "%s@\n(* returned %s *)@\n" c (show_res r);
          aux ppf xs
      | _ -> assert false
    in
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
      inits aux trace

  let pp_terms ppf err =
    let open Fmt in
    let pp_aux ppf (term, l) = pf ppf "@[%a@\n  @[%s@]@]@\n" pp_loc l term in
    pf ppf "%a" (list ~sep:(any "@\n") pp_aux) err

  let message max_suts trace report =
    Test.fail_reportf
      "Gospel specification violation in function %s\n\
       @;\
      \  @[%a@]@\n\
       when executing the following sequence of operations:@\n\
       @;\
      \  @[%a@]@." report.cmd pp_terms report.terms (pp_trace max_suts)
      (trace, report.mod_name, report.init_sut, report.ret)

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
            | Some (rest, report) -> Some ((call, res) :: rest, report))
        | Some report -> Some ([ (call, res) ], report))

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

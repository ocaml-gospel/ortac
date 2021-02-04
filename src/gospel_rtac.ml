open Ppxlib
open Gospel
open Fmt
open Builder

exception Unsupported of Location.t option * string

let lident s = noloc (lident s)

let string_of_exp : Tterm.term_node -> Tterm.Ident.t option = function
  | Tvar x -> Some x.vs_name
  | _ -> None

let rec array_no_coercion (ls : Tterm.lsymbol) (tlist : Tterm.term list) =
  (match ls.ls_name.id_str with
  | "mixfix [_]" -> Some "Array.get"
  | "length" -> Some "Array.length"
  | _ -> None)
  |> Option.map (fun f ->
         match (List.hd tlist).t_node with
         | Tapp (elts, [ arr ]) when elts.ls_name.id_str = "elts" ->
             Some (eapply (evar f) (term arr :: List.map term (List.tl tlist)))
         | _ -> None)
  |> Option.join

and bounds (t : Tterm.term) : (Tterm.Ident.t * expression * expression) option =
  let comb ~right (f : Tterm.lsymbol) e =
    match f.ls_name.id_str with
    | "infix >=" -> if right then (Some e, None) else (None, Some e)
    | "infix <=" -> if right then (None, Some e) else (Some e, None)
    | "infix <" ->
        if right then (None, Some (epred e)) else (Some (esucc e), None)
    | "infix >" ->
        if right then (Some (esucc e), None) else (None, Some (epred e))
    | _ -> (None, None)
  in
  let ( @+ ) a (b, c) = (a, b, c) in
  let bound = function
    | Tterm.Tapp (f, [ x1; x2 ]) -> (
        let sx1 = string_of_exp x1.t_node in
        let sx2 = string_of_exp x2.t_node in
        match (sx1, sx2) with
        | Some _, Some _ | None, None -> (None, None, None)
        | sx1, None ->
            let e2 = term x2 in
            sx1 @+ comb ~right:true f e2
        | None, sx2 ->
            let e1 = term x1 in
            sx2 @+ comb ~right:false f e1)
    | _ -> (None, None, None)
  in
  match t.t_node with
  | Tbinop (Tand, t1, t2) -> (
      match (bound t1.t_node, bound t2.t_node) with
      | (Some x, None, Some eupper), (Some x', Some elower, None)
      | (Some x, Some elower, None), (Some x', None, Some eupper)
        when x = x' ->
          Some (x, elower, eupper)
      | _, _ -> None
      | exception Unsupported _ -> None)
  | _ -> None

and term (t : Tterm.term) : expression =
  let unsupported m = raise (Unsupported (t.t_loc, m)) in
  match t.t_node with
  | Tvar { vs_name; _ } -> evar (str "%a" Identifier.Ident.pp vs_name)
  | Tconst c -> econst c
  | Tapp (fs, []) when Tterm.(ls_equal fs fs_bool_true) -> [%expr true]
  | Tapp (fs, []) when Tterm.(ls_equal fs fs_bool_false) -> [%expr false]
  | Tapp (ls, tlist) -> (
      match array_no_coercion ls tlist with
      | Some e -> e
      | None -> (
          Drv.find_opt ls.ls_name.id_str |> function
          | Some f -> eapply (evar f) (List.map term tlist)
          | None ->
              kstr unsupported "function application `%s`" ls.ls_name.id_str))
  | Tif (i, t, e) -> [%expr if [%e term i] then [%e term t] else [%e term e]]
  | Tlet (x, t1, t2) ->
      let x = str "%a" Identifier.Ident.pp x.vs_name in
      [%expr
        let [%p pvar x] = [%e term t1] in
        [%e term t2]]
  | Tcase (_, _) -> unsupported "case filtering"
  | Tquant (quant, _vars, _, t) -> (
      match quant with
      | Tterm.Tforall | Tterm.Texists -> (
          let op = if quant = Tforall then "forall" else "exists" in
          match t.t_node with
          | Tbinop (Timplies, t1, t2) -> (
              bounds t1 |> function
              | None -> unsupported "forall"
              | Some (x, start, stop) ->
                  let t2 = term t2 in
                  let x = str "%a" Identifier.Ident.pp x in
                  let func = pexp_fun Nolabel None (pvar x) t2 in
                  eapply (evar (str "Z.%s" op)) [ start; stop; func ])
          | Tterm.Ttrue -> [%expr true]
          | Tterm.Tfalse -> [%expr false]
          | _ -> unsupported op)
      | Tterm.Tlambda -> unsupported "lambda quantification")
  | Tbinop (op, t1, t2) -> (
      match op with
      | Tterm.Tand ->
          let vt1 = gen_symbol ~prefix:"__t1" () in
          let vt2 = gen_symbol ~prefix:"__t2" () in
          [%expr
            let [%p pvar vt1] = [%e term t1] in
            let [%p pvar vt2] = [%e term t2] in
            [%e evar vt1] && [%e evar vt2]]
      | Tterm.Tand_asym -> [%expr [%e term t1] && [%e term t2]]
      | Tterm.Tor ->
          let vt1 = gen_symbol ~prefix:"__t1" () in
          let vt2 = gen_symbol ~prefix:"__t2" () in
          [%expr
            let [%p pvar vt1] = [%e term t1] in
            let [%p pvar vt2] = [%e term t2] in
            [%e evar vt1] || [%e evar vt2]]
      | Tterm.Tor_asym -> [%expr [%e term t1] || [%e term t2]]
      | Tterm.Timplies -> [%expr (not [%e term t1]) || [%e term t2]]
      | Tterm.Tiff -> [%expr [%e term t1] = [%e term t2]])
  | Tnot t -> [%expr not [%e term t]]
  | Told _ -> unsupported "old operator"
  | Ttrue -> [%expr true]
  | Tfalse -> [%expr false]

let term fail t = [%expr try [%e term t] with _ as e -> [%e fail (evar "e")]]

let check_term_in name fail_violated fail_nonexec args t next =
  let expr =
    [%expr if not [%e term fail_nonexec t] then [%e fail_violated ()]]
  in
  let func = efun args expr in
  [%expr
    let [%p pvar name] = [%e func] in
    [%e next]]

let post names fun_name loc rets posts expr =
  List.fold_right2
    (fun name t exp ->
      let fail_violated () = failed_post fun_name loc t in
      let fail_nonexec e = failed_post_nonexec e fun_name loc t in
      let f =
        check_term_in name fail_violated fail_nonexec [ (Nolabel, rets) ] t
      in
      f exp)
    names posts expr

let pre names loc fun_name eloc args pres expr =
  List.fold_right2
    (fun name (t, check) exp ->
      if check then raise (Unsupported (Some loc, "`check` condition"));
      let fail_violated () = failed_pre fun_name eloc t in
      let fail_nonexec e = failed_pre_nonexec e fun_name eloc t in
      let f = check_term_in name fail_violated fail_nonexec args t in
      f exp)
    names pres expr

let rec xpost_pattern exn = function
  | Tterm.Pwild -> ppat_construct (lident exn) (Some ppat_any)
  | Tterm.Pvar x ->
      ppat_construct (lident exn)
        (Some (ppat_var (noloc (str "%a" Tterm.Ident.pp x.vs_name))))
  | Tterm.Papp (ls, []) when Tterm.(ls_equal ls (fs_tuple 0)) -> pvar exn
  | Tterm.Papp (_ls, _l) -> assert false
  | Tterm.Por (p1, p2) ->
      ppat_or (xpost_pattern exn p1.p_node) (xpost_pattern exn p2.p_node)
  | Tterm.Pas (p, s) ->
      ppat_alias
        (xpost_pattern exn p.p_node)
        (noloc (str "%a" Tterm.Ident.pp s.vs_name))

let xpost_guard _loc fun_name eloc xpost call =
  let module M = Map.Make (struct
    type t = Ttypes.xsymbol

    let compare = compare
  end) in
  let default_cases =
    [
      case ~guard:None
        ~lhs:[%pat? (Out_of_memory | Stack_overflow) as e]
        ~rhs:[%expr raise e];
      case ~guard:None
        ~lhs:[%pat? _ as e]
        ~rhs:[%expr unexpected_exn [%e eloc] [%e estring fun_name] e];
    ]
  in
  let assert_false_case =
    case ~guard:None ~lhs:[%pat? _] ~rhs:[%expr assert false]
  in
  List.fold_left
    (fun map (exn, ptlist) ->
      let name = exn.Ttypes.xs_ident.id_str in
      let cases =
        List.rev_map
          (fun (p, t) ->
            let fail_nonexec e = failed_xpost_nonexec e fun_name eloc t in
            case ~guard:None
              ~lhs:(xpost_pattern name p.Tterm.p_node)
              ~rhs:
                [%expr
                  if not [%e term fail_nonexec t] then
                    [%e failed_post fun_name eloc t]])
          ptlist
        @ [ assert_false_case ]
      in
      M.update exn
        (function None -> Some [ cases ] | Some e -> Some (cases :: e))
        map)
    M.empty xpost
  |> fun cases ->
  M.fold
    (fun exn cases acc ->
      let name = exn.Ttypes.xs_ident.id_str in
      let has_args = exn.Ttypes.xs_type <> Ttypes.Exn_tuple [] in
      let alias = gen_symbol ~prefix:"__e" () in
      let rhs =
        [%expr
          [%e List.map (pexp_match (evar alias)) cases |> esequence];
          raise [%e evar alias]]
      in
      let lhs =
        ppat_alias
          (ppat_construct (lident name)
             (if has_args then Some ppat_any else None))
          (noloc alias)
      in
      case ~guard:None ~lhs ~rhs :: acc)
    cases default_cases
  |> pexp_try call

let of_gospel_args args =
  let to_string x = str "%a" Tast.Ident.pp x.Tterm.vs_name in
  List.fold_right
    (fun arg (eargs, pargs) ->
      match arg with
      | Tast.Lnone x ->
          let s = to_string x in
          ((Nolabel, evar s) :: eargs, (Nolabel, pvar s) :: pargs)
      | Tast.Lquestion x ->
          let s = to_string x in
          ((Optional s, evar s) :: eargs, (Nolabel, pvar s) :: pargs)
      | Tast.Lnamed x ->
          let s = to_string x in
          ((Labelled s, evar s) :: eargs, (Labelled s, pvar s) :: pargs)
      | Tast.Lghost _ -> (eargs, pargs))
    args ([], [])

let returned_pattern rets =
  let to_string x = str "%a" Tast.Ident.pp x.Tterm.vs_name in
  List.filter_map
    (function
      | Tast.Lnone x -> Some (pvar (to_string x))
      | Tast.Lghost _ -> None
      | Tast.Lquestion _ | Tast.Lnamed _ -> assert false)
    rets
  |> ppat_tuple

let value (val_desc : Tast.val_description) =
  let process (spec : Tast.val_spec) =
    (* Declaration location *)
    let loc = val_desc.vd_loc in
    if List.length spec.sp_args = 0 then
      raise (Unsupported (Some loc, "non-function value"));
    (* Arguments *)
    let eargs, pargs = of_gospel_args spec.sp_args in
    (* Returned pattern *)
    let prets = returned_pattern spec.sp_ret in
    let ret_name = gen_symbol ~prefix:"__ret" () in
    let loc_name = gen_symbol ~prefix:"__loc" () in
    let let_loc next =
      [%expr
        let [%p pvar loc_name] = [%e elocation loc] in
        [%e next]]
    in
    let eloc = evar loc_name in
    let pre_names =
      List.init (List.length spec.sp_pre) (fun _ ->
          gen_symbol ~prefix:"__check_pre" ())
    in
    let post_names =
      List.init (List.length spec.sp_post) (fun _ ->
          gen_symbol ~prefix:"__check_post" ())
    in
    let let_posts =
      post post_names val_desc.vd_name.id_str eloc prets spec.sp_post
    in
    let let_pres =
      pre pre_names loc val_desc.vd_name.id_str eloc pargs spec.sp_pre
    in
    let post_checks =
      List.map (fun s -> eapply (evar s) [ evar ret_name ]) post_names
      |> esequence
    in
    let pre_checks =
      List.map (fun s -> pexp_apply (evar s) eargs) pre_names |> esequence
    in
    let call = pexp_apply (evar val_desc.vd_name.id_str) eargs in
    let check_raises =
      xpost_guard loc val_desc.vd_name.id_str eloc spec.sp_xpost call
    in
    let let_call next =
      [%expr
        let [%p pvar ret_name] = [%e check_raises] in
        [%e next]]
    in
    let return = evar ret_name in
    let body =
      efun pargs @@ let_loc @@ let_posts @@ let_pres
      @@ pexp_sequence pre_checks (let_call @@ pexp_sequence post_checks return)
    in
    [%stri let [%p pvar val_desc.vd_name.id_str] = [%e body]]
  in
  Option.map process val_desc.vd_spec

let signature =
  List.filter_map (fun (sig_item : Tast.signature_item) ->
      match sig_item.sig_desc with
      | Sig_val (decl, _ghost) -> value decl
      | _ -> None)

let module_name_of_path p =
  Filename.basename p |> Filename.chop_extension |> String.capitalize_ascii

let type_check load_path name sigs =
  let md = Tmodule.init_muc name in
  let penv =
    module_name_of_path name |> Utils.Sstr.singleton |> Typing.penv load_path
  in
  List.fold_left (Typing.type_sig_item penv) md sigs |> Tmodule.wrap_up_muc
  |> fun file -> file.fl_sigs

let main path =
  set_style_renderer stderr `Ansi_tty;
  let module_name = module_name_of_path path in
  let ast = Parser_frontend.parse_ocaml_gospel path in
  let tast = type_check [] path ast in
  try
    let open_runtime = [%stri open Gospel_runtime] in
    let include_lib =
      pmod_ident (lident module_name) |> include_infos |> pstr_include
    in
    let declarations = signature tast in
    open_runtime :: include_lib :: declarations |> Pprintast.structure stdout
  with
  | Unsupported (loc, msg) ->
      let open Fmt in
      Location.raise_errorf ?loc "%a: unsupported %s"
        (styled `Red string)
        "Error" msg
  | e -> raise e

(* Error reporting *)

let colour_of_kind = function
  | `Ok -> `Green
  | `Error -> `Red
  | `Warning -> `Yellow

let string_of_kind = function
  | `Ok -> "Success"
  | `Error -> "Error"
  | `Warning -> "Warning"

let _pp_kind ppf kind =
  let colour = colour_of_kind kind in
  let msg = string_of_kind kind in
  (styled colour string) ppf msg

(* Command line interface *)

open Cmdliner

let ocaml_file =
  let parse s =
    match Sys.file_exists s with
    | true ->
        if Sys.is_directory s || Filename.extension s <> ".mli" then
          `Error (Printf.sprintf "Error: `%s' is not an OCaml interface file" s)
        else `Ok s
    | false -> `Error (Printf.sprintf "Error: `%s' not found" s)
  in
  Arg.(
    required
    & pos 0 (some (parse, Format.pp_print_string)) None
    & info [] ~docv:"FILE")

let cmd =
  let doc = "Run GOSPEL-RTAC." in
  (Term.(const main $ ocaml_file), Term.info "gospel-rtac" ~doc)

let () = Term.(exit @@ eval cmd)

open Ppxlib
open Gospel
open Fmt
module B = Builder

exception Unsupported of Location.t option * string

let lident s = B.noloc (lident s)

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
             Some
               (B.eapply (B.evar f) (term arr :: List.map term (List.tl tlist)))
         | _ -> None)
  |> Option.join

and bounds (t : Tterm.term) : (Tterm.Ident.t * expression * expression) option =
  let comb ~right (f : Tterm.lsymbol) e =
    match f.ls_name.id_str with
    | "infix >=" -> if right then (Some e, None) else (None, Some e)
    | "infix <=" -> if right then (None, Some e) else (Some e, None)
    | "infix <" ->
        if right then (None, Some (B.epred e)) else (Some (B.esucc e), None)
    | "infix >" ->
        if right then (Some (B.esucc e), None) else (None, Some (B.epred e))
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
  | Tvar { vs_name; _ } -> B.evar (str "%a" Identifier.Ident.pp vs_name)
  | Tconst c -> B.econst c
  | Tapp (ls, tlist) -> (
      match array_no_coercion ls tlist with
      | Some e -> e
      | None -> (
          Drv.find_opt ls.ls_name.id_str |> function
          | Some f -> B.eapply (B.evar f) (List.map term tlist)
          | None ->
              kstr unsupported "function application `%s`" ls.ls_name.id_str))
  | Tif (i, t, e) ->
      let i = term i in
      let t = term t in
      let e = term e in
      B.pexp_ifthenelse i t (Some e)
  | Tlet (x, t1, t2) ->
      B.pexp_let Nonrecursive
        [ B.value_binding ~pat:(B.pvar x.vs_name.id_str) ~expr:(term t1) ]
        (term t2)
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
                  let func = B.pexp_fun Nolabel None (B.pvar x) t2 in
                  B.eapply (B.evar (str "Z.%s" op)) [ start; stop; func ])
          | Tterm.Ttrue -> B.ebool true
          | Tterm.Tfalse -> B.ebool false
          | _ -> unsupported op)
      | Tterm.Tlambda -> unsupported "lambda quantification")
  | Tbinop (op, t1, t2) -> (
      match op with
      | Tterm.Tand ->
          let vt1 = gen_symbol ~prefix:"__t1" () in
          let vt2 = gen_symbol ~prefix:"__t2" () in
          B.pexp_let Nonrecursive
            [ B.value_binding ~pat:(B.pvar vt1) ~expr:(term t1) ]
            (B.pexp_let Nonrecursive
               [ B.value_binding ~pat:(B.pvar vt2) ~expr:(term t2) ]
               (B.eand (B.evar vt1) (B.evar vt2)))
      | Tterm.Tand_asym -> B.eand (term t1) (term t2)
      | Tterm.Tor ->
          let vt1 = gen_symbol ~prefix:"__t1" () in
          let vt2 = gen_symbol ~prefix:"__t2" () in
          B.pexp_let Nonrecursive
            [ B.value_binding ~pat:(B.pvar vt1) ~expr:(term t1) ]
            (B.pexp_let Nonrecursive
               [ B.value_binding ~pat:(B.pvar vt2) ~expr:(term t2) ]
               (B.eor (B.evar vt1) (B.evar vt2)))
      | Tterm.Tor_asym -> B.eor (term t1) (term t2)
      | Tterm.Timplies ->
          let t1 = term t1 in
          let t2 = term t2 in
          B.pexp_ifthenelse t1 t2 (Some (B.ebool true))
      | Tterm.Tiff ->
          let t1 = term t1 in
          let t2 = term t2 in
          B.eapply (B.evar "=") [ t1; t2 ])
  | Tnot t -> B.enot (term t)
  | Told _ -> unsupported "old operator"
  | Ttrue -> B.ebool true
  | Tfalse -> B.ebool false

let check_term_in name fail_violated fail_nonexec args t =
  let trywith =
    let exn_alias = gen_symbol ~prefix:"__e" () in
    B.pexp_try (term t)
      [
        B.case ~guard:None
          ~lhs:(B.ppat_alias B.ppat_any (B.noloc exn_alias))
          ~rhs:(fail_nonexec (B.evar exn_alias));
      ]
    (* try t with _ as e -> <B.failed_post_nonexec e fun_name t> *)
  in
  let expr =
    B.pexp_ifthenelse (B.enot trywith) (fail_violated ()) None
    (* if not <trywith> then <B.failed_post fun_name t> *)
  in
  let func = B.efun args expr in
  let binding = B.value_binding ~pat:B.(pvar name) ~expr:func in
  B.pexp_let Nonrecursive [ binding ]

let post names fun_name rets posts expr =
  List.fold_right2
    (fun name t exp ->
      let fail_violated () = B.failed_post fun_name t in
      let fail_nonexec e = B.failed_post_nonexec e fun_name t in
      let f =
        check_term_in name fail_violated fail_nonexec [ (Nolabel, rets) ] t
      in
      f exp)
    names posts expr

let pre names loc fun_name args pres expr =
  List.fold_right2
    (fun name (t, check) exp ->
      if check then raise (Unsupported (Some loc, "`check` condition"));
      let fail_violated () = B.failed_post fun_name t in
      let fail_nonexec e = B.failed_post_nonexec e fun_name t in
      let f = check_term_in name fail_violated fail_nonexec args t in
      f exp)
    names pres expr

let rec xpost_pattern exn = function
  | Tterm.Pwild -> B.ppat_construct (lident exn) (Some B.ppat_any)
  | Tterm.Pvar x ->
      B.ppat_construct (lident exn)
        (Some (B.ppat_var (B.noloc (str "%a" Tterm.Ident.pp x.vs_name))))
  | Tterm.Papp (ls, []) when Tterm.(ls_equal ls (fs_tuple 0)) -> B.pvar exn
  | Tterm.Papp (_ls, _l) -> assert false
  | Tterm.Por (p1, p2) ->
      B.ppat_or (xpost_pattern exn p1.p_node) (xpost_pattern exn p2.p_node)
  | Tterm.Pas (p, s) ->
      B.ppat_alias
        (xpost_pattern exn p.p_node)
        (B.noloc (str "%a" Tterm.Ident.pp s.vs_name))

let xpost loc fun_name xpost =
  Ttypes.Mxs.bindings xpost
  |> List.map (fun (exn, pl) ->
         if List.length pl > 1 then
           raise
             (Unsupported
                ( Some loc,
                  "multiple exception patterns with the same constructor" ))
         else
           let exn = exn.Ttypes.xs_ident.id_str in
           let alias = gen_symbol ~prefix:"__e" () in
           List.map
             (fun (p, t) ->
               B.case ~guard:None
                 ~lhs:
                   (B.ppat_alias
                      (xpost_pattern exn p.Tterm.p_node)
                      (B.noloc alias))
                 ~rhs:
                   (B.pexp_ifthenelse (term t)
                      (B.eapply (B.evar "raise") [ B.evar alias ])
                      (Some (B.failed_xpost fun_name t))))
             pl)
  |> List.flatten

let of_gospel_args args =
  let to_string x = str "%a" Tast.Ident.pp x.Tterm.vs_name in
  List.fold_right
    (fun arg (eargs, pargs) ->
      match arg with
      | Tast.Lnone x ->
          let s = to_string x in
          ((Nolabel, B.evar s) :: eargs, (Nolabel, B.pvar s) :: pargs)
      | Tast.Lquestion x ->
          let s = to_string x in
          ((Optional s, B.evar s) :: eargs, (Nolabel, B.pvar s) :: pargs)
      | Tast.Lnamed x ->
          let s = to_string x in
          ((Labelled s, B.evar s) :: eargs, (Labelled s, B.pvar s) :: pargs)
      | Tast.Lghost _ -> (eargs, pargs))
    args ([], [])

let returned_pattern rets =
  let to_string x = str "%a" Tast.Ident.pp x.Tterm.vs_name in
  List.filter_map
    (function
      | Tast.Lnone x -> Some (B.pvar (to_string x))
      | Tast.Lghost _ -> None
      | Tast.Lquestion _ | Tast.Lnamed _ -> assert false)
    rets
  |> B.ppat_tuple

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
    let pre_names =
      List.init (List.length spec.sp_pre) (fun _ ->
          gen_symbol ~prefix:"__check_pre" ())
    in
    let post_names =
      List.init (List.length spec.sp_post) (fun _ ->
          gen_symbol ~prefix:"__check_post" ())
    in
    let let_posts =
      post post_names val_desc.vd_name.id_str prets spec.sp_post
    in
    let let_pres =
      pre pre_names loc val_desc.vd_name.id_str pargs spec.sp_pre
    in
    let post_checks =
      List.map (fun s -> B.eapply (B.evar s) [ B.evar ret_name ]) post_names
      |> B.esequence
    in
    let pre_checks =
      List.map (fun s -> B.pexp_apply (B.evar s) eargs) pre_names |> B.esequence
    in
    let call = B.pexp_apply (B.evar val_desc.vd_name.id_str) eargs in
    let check_raises =
      let cases = xpost loc val_desc.vd_name.id_str spec.sp_xpost in
      B.check_exceptions val_desc.vd_loc val_desc.vd_name.id_str call cases
    in
    let let_call =
      B.pexp_let Nonrecursive
        [ B.value_binding ~pat:(B.pvar ret_name) ~expr:check_raises ]
    in
    let return = B.evar ret_name in
    let body =
      B.efun pargs @@ let_posts @@ let_pres
      @@ B.pexp_sequence pre_checks
           (let_call @@ B.pexp_sequence post_checks return)
    in
    B.pstr_value Nonrecursive
      [ B.value_binding ~pat:(B.pvar val_desc.vd_name.id_str) ~expr:body ]
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
    let open_runtime =
      B.open_infos
        ~expr:(B.pmod_ident (lident "Gospel_runtime"))
        ~override:Fresh
      |> B.pstr_open
    in
    let include_lib =
      B.pmod_ident (lident module_name) |> B.include_infos |> B.pstr_include
    in
    let declarations = signature tast in
    open_runtime :: include_lib :: declarations |> Pprintast.structure stdout
  with
  | Unsupported (loc, msg) ->
      let open Fmt in
      let loc = Option.value ~default:Location.none loc in
      let pp_loc = (fun ppf () -> Location.print ppf loc) |> styled `Bold in
      let pp_details ppf () =
        pf ppf "%a: unsupported %s" (styled `Red string) "Error" msg
      in
      epr "%a%a@\n" pp_loc () pp_details ();
      exit 1
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

open Ppxlib
open Fmt
module B = Builder

exception Unsupported of Location.t option * string

let string_of_exp : Gospel.Tterm.term_node -> Gospel.Tterm.Ident.t option =
  function
  | Tvar x -> Some x.vs_name
  | _ -> None

let rec array_no_coercion (ls : Gospel.Tterm.lsymbol)
    (tlist : Gospel.Tterm.term list) =
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

and bounds (t : Gospel.Tterm.term) :
    (Gospel.Tterm.Ident.t * expression * expression) option =
  let comb ~right (f : Gospel.Tterm.lsymbol) e =
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
    | Gospel.Tterm.Tapp (f, [ x1; x2 ]) -> (
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

and term (t : Gospel.Tterm.term) : expression =
  let unsupported m = raise (Unsupported (t.t_loc, m)) in
  match t.t_node with
  | Tvar { vs_name; _ } -> B.evar (str "%a" Gospel.Identifier.Ident.pp vs_name)
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
      | Gospel.Tterm.Tforall | Gospel.Tterm.Texists -> (
          let op = if quant = Tforall then "forall" else "exists" in
          match t.t_node with
          | Tbinop (Timplies, t1, t2) -> (
              bounds t1 |> function
              | None -> unsupported "forall"
              | Some (x, start, stop) ->
                  let t2 = term t2 in
                  let x = str "%a" Gospel.Identifier.Ident.pp x in
                  let func = B.pexp_fun Nolabel None (B.pvar x) t2 in
                  B.eapply (B.evar (str "Z.%s" op)) [ start; stop; func ])
          | Gospel.Tterm.Ttrue -> B.ebool true
          | Gospel.Tterm.Tfalse -> B.ebool false
          | _ -> unsupported op)
      | Gospel.Tterm.Tlambda -> unsupported "lambda quantification")
  | Tbinop (op, t1, t2) -> (
      match op with
      | Gospel.Tterm.Tand ->
          let vt1 = gen_symbol ~prefix:"__t1" () in
          let vt2 = gen_symbol ~prefix:"__t2" () in
          B.pexp_let Nonrecursive
            [ B.value_binding ~pat:(B.pvar vt1) ~expr:(term t1) ]
            (B.pexp_let Nonrecursive
               [ B.value_binding ~pat:(B.pvar vt2) ~expr:(term t2) ]
               (B.eand (B.evar vt1) (B.evar vt2)))
      | Gospel.Tterm.Tand_asym -> B.eand (term t1) (term t2)
      | Gospel.Tterm.Tor ->
          let vt1 = gen_symbol ~prefix:"__t1" () in
          let vt2 = gen_symbol ~prefix:"__t2" () in
          B.pexp_let Nonrecursive
            [ B.value_binding ~pat:(B.pvar vt1) ~expr:(term t1) ]
            (B.pexp_let Nonrecursive
               [ B.value_binding ~pat:(B.pvar vt2) ~expr:(term t2) ]
               (B.eor (B.evar vt1) (B.evar vt2)))
      | Gospel.Tterm.Tor_asym -> B.eor (term t1) (term t2)
      | Gospel.Tterm.Timplies ->
          let t1 = term t1 in
          let t2 = term t2 in
          B.pexp_ifthenelse t1 t2 (Some (B.ebool true))
      | Gospel.Tterm.Tiff ->
          let t1 = term t1 in
          let t2 = term t2 in
          B.eapply (B.evar "=") [ t1; t2 ])
  | Tnot t -> B.enot (term t)
  | Told _ -> unsupported "old operator"
  | Ttrue -> B.ebool true
  | Tfalse -> B.ebool false

let pp_args = list ~sep:sp (parens Gospel.Tast.print_lb_arg)

let check_pre fun_name args t =
  let translated_ite ppf =
    B.pexp_ifthenelse (B.enot (term t)) (B.failed_pre fun_name t) None
    |> Pprintast.expression ppf
  in
  let name = gen_symbol ~prefix:"__check" () in
  ( name,
    str
      {|let %s %a =@
    try@
      %t@
    with@
    | Gospel_runtime.Error _ as e -> raise e@
    | _ as e -> %a@
  in|}
      name pp_args args translated_ite Pprintast.expression
      (B.failed_pre_nonexec (B.evar "e") fun_name t) )

let check_post fun_name ret t =
  let translated_ite ppf =
    B.pexp_ifthenelse (B.enot (term t)) (B.failed_post fun_name t) None
    |> Pprintast.expression ppf
  in
  let name = gen_symbol ~prefix:"__check" () in
  ( name,
    str
      {|let %s %a =@
    try@
      %t@
    with@
    | Gospel_runtime.Error _ as e -> raise e@
    | _ as e -> %a@
  in|}
      name
      (parens Gospel.Tast.print_lb_arg)
      ret translated_ite Pprintast.expression
      (B.failed_post_nonexec (B.evar "e") fun_name t) )

let post fun_name ret = List.map (check_post fun_name ret)

let pre fun_name args =
  List.map (fun (t, b) ->
      if not b then check_pre fun_name args t else assert false (*TODO*))

let rec xpost_pattern exn = function
  | Gospel.Tterm.Pwild ->
      B.ppat_construct (B.noloc (lident exn)) (Some B.ppat_any)
  | Gospel.Tterm.Pvar x ->
      B.ppat_construct
        (B.noloc (lident exn))
        (Some (B.ppat_var (B.noloc (str "%a" Gospel.Tterm.Ident.pp x.vs_name))))
  | Gospel.Tterm.Papp (ls, []) when Gospel.Tterm.(ls_equal ls (fs_tuple 0)) ->
      B.pvar exn
  | Gospel.Tterm.Papp (_ls, _l) -> assert false
  | Gospel.Tterm.Por (p1, p2) ->
      B.ppat_or (xpost_pattern exn p1.p_node) (xpost_pattern exn p2.p_node)
  | Gospel.Tterm.Pas (p, s) ->
      B.ppat_alias
        (xpost_pattern exn p.p_node)
        (B.noloc (str "%a" Gospel.Tterm.Ident.pp s.vs_name))

let xpost loc fun_name xpost =
  Gospel.Ttypes.Mxs.bindings xpost
  |> List.map (fun (exn, pl) ->
         if List.length pl > 1 then
           raise
             (Unsupported
                (loc, "multiple exception patterns with the same constructor"))
         else
           let exn = exn.Gospel.Ttypes.xs_ident.id_str in
           let alias = gen_symbol ~prefix:"__e" () in
           List.map
             (fun (p, t) ->
               B.case ~guard:None
                 ~lhs:
                   (B.ppat_alias
                      (xpost_pattern exn p.Gospel.Tterm.p_node)
                      (B.noloc alias))
                 ~rhs:
                   (B.pexp_ifthenelse (term t)
                      (B.eapply (B.evar "raise") [ B.evar alias ])
                      (Some (B.failed_xpost fun_name t))))
             pl)
  |> List.flatten

let filter_ghost =
  List.filter (function Gospel.Tast.Lghost _ -> false | _ -> true)

let arg =
  let to_string x = str "%a" Gospel.Tast.Ident.pp x.Gospel.Tterm.vs_name in
  function
  | Gospel.Tast.Lnone x -> (Nolabel, B.evar (to_string x))
  | Gospel.Tast.Lquestion x ->
      let s = to_string x in
      (Optional s, B.evar s)
  | Gospel.Tast.Lnamed x ->
      let s = to_string x in
      (Labelled s, B.evar s)
  | Gospel.Tast.Lghost _ -> assert false

let value loc (val_desc : Gospel.Tast.val_description) : string option =
  let process (spec : Gospel.Tast.val_spec) =
    let ninputs = List.length spec.sp_args in
    if ninputs >= 1 then
      let ret_name =
        filter_ghost spec.sp_ret |> function
        | [ arg ] -> arg
        | _ -> raise (Unsupported (loc, "returned pattern"))
      in
      let posts = post val_desc.vd_name.id_str ret_name spec.sp_post in
      let pres = pre val_desc.vd_name.id_str spec.sp_args spec.sp_pre in
      let post_checks =
        List.map
          (fun p ->
            str "%s %a;" (fst p) (parens Gospel.Tast.print_lb_arg) ret_name)
          posts
      in
      let pre_checks =
        List.map (fun p -> str "%s %a;" (fst p) pp_args spec.sp_args) pres
      in
      let call =
        filter_ghost spec.sp_args |> List.map arg
        |> B.pexp_apply (B.evar val_desc.vd_name.id_str)
      in
      let check_exn =
        let cases = xpost loc val_desc.vd_name.id_str spec.sp_xpost in
        (*XXX: [raises] should be made out of the xpost conditions. *)
        B.check_exceptions val_desc.vd_loc val_desc.vd_name.id_str call cases
      in
      str {|let %a %a =@
  %a@
  %a@
  %a@
  let %a = %a in@
  %a@
  %a|}
        Gospel.Identifier.Ident.pp val_desc.vd_name pp_args spec.sp_args
        (list ~sep:(any "@\n") string)
        (List.map snd pres)
        (list ~sep:(any "@\n") string)
        (List.map snd posts) (list ~sep:sp string) pre_checks
        (parens Gospel.Tast.print_lb_arg)
        ret_name Pprintast.expression check_exn (list ~sep:sp string)
        post_checks
        (parens Gospel.Tast.print_lb_arg)
        ret_name
    else raise (Unsupported (loc, "non-function value"))
  in
  Option.map process val_desc.vd_spec

let signature (ast : Gospel.Tast.signature) : string list =
  List.filter_map
    (fun (sig_item : Gospel.Tast.signature_item) ->
      match sig_item.sig_desc with
      | Sig_val (decl, _ghost) -> value (Some sig_item.sig_loc) decl
      | _ -> None)
    ast

let module_name_of_path p =
  Filename.basename p |> Filename.chop_extension |> String.capitalize_ascii

let type_check load_path name sigs =
  let md = Gospel.Tmodule.init_muc name in
  let penv =
    module_name_of_path name |> Gospel.Utils.Sstr.singleton
    |> Gospel.Typing.penv load_path
  in
  List.fold_left (Gospel.Typing.type_sig_item penv) md sigs
  |> Gospel.Tmodule.wrap_up_muc
  |> fun file -> file.fl_sigs

let main (path : string) : unit =
  set_style_renderer stderr `Ansi_tty;
  let module_name = module_name_of_path path in
  let ast = Gospel.Parser_frontend.parse_ocaml_gospel path in
  let tast = type_check [] path ast in
  try
    let declarations = signature tast in
    let open Fmt in
    pr "open! Gospel_runtime@\n@\ninclude %s\n@\n%a@\n" module_name
      (list ~sep:(any "@\n@\n") string)
      declarations
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

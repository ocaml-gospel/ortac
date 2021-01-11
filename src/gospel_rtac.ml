open Ppxlib

let noloc txt = { txt; loc = Location.none }

module B = struct
  include Ast_builder.Make (struct
    let loc = Location.none
  end)

  let enot e = eapply (evar "not") [ e ]

  let eand e1 e2 = eapply (evar "&&") [ e1; e2 ]

  let eor e1 e2 = eapply (evar "||") [ e1; e2 ]

  let eposition pos =
    pexp_record
      [
        (noloc (lident "pos_fname"), estring pos.pos_fname);
        (noloc (lident "pos_lnum"), eint pos.pos_lnum);
        (noloc (lident "pos_bol"), eint pos.pos_bol);
        (noloc (lident "pos_cnum"), eint pos.pos_cnum);
      ]
      None

  let elocation loc =
    pexp_record
      [
        (noloc (lident "loc_start"), eposition loc.loc_start);
        (noloc (lident "loc_end"), eposition loc.loc_end);
        (noloc (lident "loc_ghost"), ebool loc.loc_ghost);
      ]
      None
end

exception Unsupported of Gospel.Location.t option * string

let const : Gospel.Oasttypes.constant -> expression = function
  | Pconst_integer (c, o) ->
      Pconst_integer (c, o) |> B.pexp_constant |> fun e ->
      B.eapply (B.evar "Z.of_int") [ e ]
  | Pconst_char c -> B.echar c
  | Pconst_string (s, d) ->
      Pconst_string (s, Location.none, d) |> B.pexp_constant
  | Pconst_float (c, o) -> Pconst_float (c, o) |> B.pexp_constant

let string_of_exp : Gospel.Tterm.term_node -> string option = function
  | Tvar x -> Some x.vs_name.id_str
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

and exp_of_const (t : Gospel.Tterm.term) : expression =
  match t.t_node with
  | Tconst c -> const c
  | Tapp (f, c) when f.ls_name.id_str = "integer_of_int" ->
      B.eapply (B.evar "Z.of_int") [ List.hd c |> term ]
  | _ -> Fmt.invalid_arg "not a constant:@\n%a%!" Gospel.Upretty_printer.term t

and bounds (t : Gospel.Tterm.term) : (string * expression * expression) option =
  let pred e = B.eapply (B.evar "Z.pred") [ e ] in
  let succ e = B.eapply (B.evar "Z.succ") [ e ] in
  let comb ~right (f : Gospel.Tterm.lsymbol) e =
    match f.ls_name.id_str with
    | "infix >=" -> if right then (Some e, None) else (None, Some e)
    | "infix <=" -> if right then (None, Some e) else (Some e, None)
    | "infix <" -> if right then (None, Some (pred e)) else (Some (succ e), None)
    | "infix >" -> if right then (Some (succ e), None) else (None, Some (pred e))
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
            let e2 = exp_of_const x2 in
            sx1 @+ comb ~right:true f e2
        | None, sx2 ->
            let e1 = exp_of_const x1 in
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
      | _, _ -> None)
  | _ -> None

and term (t : Gospel.Tterm.term) : expression =
  let unsupported m = raise (Unsupported (t.t_loc, m)) in
  match t.t_node with
  | Tvar { vs_name; _ } ->
      B.evar (Fmt.str "%a" Gospel.Identifier.Ident.pp vs_name)
  | Tconst c -> const c
  | Tapp (ls, tlist) -> (
      match array_no_coercion ls tlist with
      | Some e -> e
      | None -> (
          Drv.find_opt ls.ls_name.id_str |> function
          | Some f -> B.eapply (B.evar f) (List.map term tlist)
          | None ->
              Fmt.kstr unsupported "function application `%s`" ls.ls_name.id_str
          ))
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
      | Gospel.Tterm.Tforall -> (
          match t.t_node with
          | Tbinop (Timplies, t1, t2) -> (
              bounds t1 |> function
              | None -> unsupported "forall"
              | Some (x, start, stop) ->
                  let t2 = term t2 in
                  let func = B.pexp_fun Nolabel None (B.pvar x) t2 in
                  B.eapply (B.evar "Z.forall") [ start; stop; func ])
          | Gospel.Tterm.Ttrue -> B.ebool true
          | Gospel.Tterm.Tfalse -> B.ebool false
          | _ -> unsupported "forall")
      | Gospel.Tterm.Texists -> unsupported "exists"
      | Gospel.Tterm.Tlambda -> unsupported "lambda quantification")
  | Tbinop (op, t1, t2) -> (
      match op with
      | Gospel.Tterm.Tand ->
          let vt1 = gen_symbol ~prefix:"_t1" () in
          let vt2 = gen_symbol ~prefix:"_t2" () in
          B.pexp_let Nonrecursive
            [ B.value_binding ~pat:(B.pvar vt1) ~expr:(term t1) ]
            (B.pexp_let Nonrecursive
               [ B.value_binding ~pat:(B.pvar vt2) ~expr:(term t2) ]
               (B.eand (B.evar vt1) (B.evar vt2)))
      | Gospel.Tterm.Tand_asym -> B.eand (term t1) (term t2)
      | Gospel.Tterm.Tor ->
          let vt1 = gen_symbol ~prefix:"_t1" () in
          let vt2 = gen_symbol ~prefix:"_t2" () in
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

let location_of_gospel_loc : Gospel.Warnings.loc option -> location = function
  | Some l ->
      { loc_start = l.loc_start; loc_end = l.loc_end; loc_ghost = l.loc_ghost }
  | None -> Location.none

let failed_post fun_name term =
  B.eapply (B.evar "bad_post")
    [
      B.pexp_open
        (B.open_infos
           ~expr:(B.pmod_ident (noloc (lident "Ppxlib.Location")))
           ~override:Fresh)
        (B.elocation (location_of_gospel_loc term.Gospel.Tterm.t_loc));
      B.estring fun_name;
      B.estring (Fmt.str "%a" Gospel.Tterm.print_term term);
    ]

let failed_nonexec fun_name term exn =
  B.eapply (B.evar "runtime_exn")
    [
      B.pexp_open
        (B.open_infos
           ~expr:(B.pmod_ident (noloc (lident "Ppxlib.Location")))
           ~override:Fresh)
        (B.elocation (location_of_gospel_loc term.Gospel.Tterm.t_loc));
      B.estring fun_name;
      B.estring (Fmt.str "%a" Gospel.Tterm.print_term term);
      exn;
    ]

let check_function fun_name ret t =
  let translated_ite ppf =
    B.pexp_ifthenelse (B.enot (term t)) (failed_post fun_name t) None
    |> Pprintast.expression ppf
  in
  let name = gen_symbol ~prefix:"__check" () in
  ( name,
    Fmt.str
      {|let %s %a =@
    try@
      %t@
    with@
    | Gospel_runtime.Error _ as e -> raise e@
    | _ as e -> %a@
  in|}
      name
      (Fmt.parens Gospel.Tast.print_lb_arg)
      ret translated_ite Pprintast.expression
      (failed_nonexec fun_name t (B.evar "e")) )

let post fun_name ret = List.map (check_function fun_name ret)

let args = Fmt.(list ~sep:sp (parens Gospel.Tast.print_lb_arg))

let filter_ghost =
  List.filter (function Gospel.Tast.Lghost _ -> false | _ -> true)

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
      let checks =
        List.map
          (fun p ->
            Fmt.str "%s %a;" (fst p)
              (Fmt.parens Gospel.Tast.print_lb_arg)
              ret_name)
          posts
      in
      Fmt.str {|let %a %a =@
  %a@
  let %a = %a %a in@
  %a@
  %a|}
        Gospel.Identifier.Ident.pp val_desc.vd_name args spec.sp_args
        Fmt.(list ~sep:(any "@\n") string)
        (List.map snd posts)
        (Fmt.parens Gospel.Tast.print_lb_arg)
        ret_name Gospel.Identifier.Ident.pp val_desc.vd_name args spec.sp_args
        Fmt.(list ~sep:sp string)
        checks
        (Fmt.parens Gospel.Tast.print_lb_arg)
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
  Fmt.(set_style_renderer stderr `Ansi_tty);
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
      let loc = Option.value ~default:Gospel.Location.none loc in
      let pp_loc =
        (fun ppf () -> Gospel.Location.print ppf loc) |> styled `Bold
      in
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
  Fmt.(styled colour string) ppf msg

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

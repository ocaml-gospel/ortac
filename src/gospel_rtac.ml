open Ppxlib

module B = Ast_builder.Make (struct
  let loc = Location.none
end)

exception Unsupported of Gospel.Location.t option * string

let rec term (t : Gospel.Tterm.term) : expression =
  let unsupported m = raise (Unsupported (t.t_loc, m)) in
  match t.t_node with
  | Tvar { vs_name; _ } -> B.evar vs_name.id_str
  | Tconst c ->
      ( match c with
      | Pconst_integer (c, o) -> Pconst_integer (c, o)
      | Pconst_char c -> Pconst_char c
      | Pconst_string (s, d) -> Pconst_string (s, d)
      | Pconst_float (c, o) -> Pconst_float (c, o) )
      |> B.pexp_constant
  | Tapp (ls, tlist) ->
      if Gospel.Identifier.is_infix ls.ls_name.id_str then
        let op =
          String.split_on_char ' ' ls.ls_name.id_str |> List.tl |> List.hd
        in
        List.map term tlist |> B.eapply (B.evar op)
      else if ls.ls_name.id_str = "integer_of_int" then List.hd tlist |> term
      else unsupported (Fmt.str "function application `%s`" ls.ls_name.id_str)
  | Tif (i, t, e) ->
      let i = term i in
      let t = term t in
      let e = term e in
      B.pexp_ifthenelse i t (Some e)
  | Tlet (_, _, _) -> unsupported "let binding"
  | Tcase (_, _) -> unsupported "case filtering"
  | Tquant (_, _, _, _) -> unsupported "quantifier"
  | Tbinop (_, _, _) -> unsupported "binary operator"
  | Tnot t -> [ term t ] |> B.eapply (B.evar "not")
  | Told _ -> unsupported "old operator"
  | Ttrue -> B.ebool true
  | Tfalse -> B.ebool false

let check_function ret i t =
  let comment ppf = Gospel.Upretty_printer.term ppf t in
  let translated ppf = term t |> Pprintast.expression ppf in
  let name = Fmt.str "check_%d" i in
  ( name,
    Fmt.str
      {|let %s %a =
    (* %t *)
    let __ret =
      %t
    in
    if not __ret then __fail ()
  in|}
      name
      (Fmt.parens Gospel.Tast.print_lb_arg)
      ret comment translated )

let post ret = List.mapi (check_function ret)

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
      let posts = post ret_name spec.sp_post in
      let checks =
        List.map
          (fun p ->
            Fmt.str "%s %a;" (fst p)
              (Fmt.parens Gospel.Tast.print_lb_arg)
              ret_name)
          posts
      in
      Fmt.str {|let %a %a =
  %a
  let %a = %a %a in
  %a
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
      | Sig_val (val_desc, _ghost) -> value (Some sig_item.sig_loc) val_desc
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
    pr "include %s@\n@\nlet __fail () = assert false@\n@\n%a@\n" module_name
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

(* Command line *)

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

open Ppxlib

include Ast_builder.Make (struct
  let loc = Location.none
end)

let noloc txt = { txt; loc = Location.none }

let enot e = eapply (evar "not") [ e ]

let eand e1 e2 = eapply (evar "&&") [ e1; e2 ]

let eor e1 e2 = eapply (evar "||") [ e1; e2 ]

let epred e = eapply (evar "Z.pred") [ e ]

let esucc e = eapply (evar "Z.succ") [ e ]

let econst = function
  | Pconst_integer (c, o) ->
      Pconst_integer (c, o) |> pexp_constant |> fun e ->
      eapply (evar "Z.of_int") [ e ]
  | _ as e -> pexp_constant e

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
  pexp_open
    (open_infos
       ~expr:(pmod_ident (noloc (lident "Ppxlib.Location")))
       ~override:Fresh)
    (pexp_record
       [
         (noloc (lident "loc_start"), eposition loc.loc_start);
         (noloc (lident "loc_end"), eposition loc.loc_end);
         (noloc (lident "loc_ghost"), ebool loc.loc_ghost);
       ]
       None)

let failed error_kind term_kind fun_name loc term =
  let func, exn =
    match error_kind with
    | `Violated -> ("violated", None)
    | `RuntimeExn e -> ("runtime_exn", Some e)
  in
  let term =
    pexp_construct
      (noloc
         (lident
            (match term_kind with
            | `Pre -> "Pre"
            | `Post -> "Post"
            | `XPost -> "XPost")))
      (Some (estring (Fmt.str "%a" Gospel.Tterm.print_term term)))
  in
  eapply (evar func)
    (match exn with
    | None -> [ loc; estring fun_name; term ]
    | Some e -> [ loc; estring fun_name; term; e ])

let failed_pre = failed `Violated `Pre

let failed_post = failed `Violated `Post

let failed_pre_nonexec exn = failed (`RuntimeExn exn) `Pre

let failed_post_nonexec exn = failed (`RuntimeExn exn) `Post

let failed_xpost = failed `Violated `XPost

let failed_xpost_nonexec exn = failed (`RuntimeExn exn) `XPost

let check_exceptions fun_name eloc call raises =
  let allowed_generic =
    case ~guard:None
      ~lhs:
        (ppat_alias
           (ppat_or
              (ppat_var (noloc "Out_of_memory"))
              (ppat_var (noloc "Stack_overflow")))
           (noloc "e"))
      ~rhs:(eapply (evar "raise") [ evar "e" ])
  in
  let generic =
    case ~guard:None
      ~lhs:(ppat_alias ppat_any (noloc "e"))
      ~rhs:(eapply (evar "unexpected_exn") [ eloc; estring fun_name; evar "e" ])
  in
  pexp_try call (raises @ [ allowed_generic; generic ])

let efun args expr =
  List.fold_right
    (fun a ->
      let label, p = a in
      pexp_fun label None p)
    args expr

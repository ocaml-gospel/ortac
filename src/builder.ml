open Ppxlib

include Ast_builder.Make (struct
  let loc = Location.none
end)

let noloc txt = { txt; loc = Location.none }

let enot e = eapply (evar "not") [ e ]

let eand e1 e2 = eapply (evar "&&") [ e1; e2 ]

let eor e1 e2 = eapply (evar "||") [ e1; e2 ]

let econst : Gospel.Oasttypes.constant -> expression = function
  | Pconst_integer (c, o) ->
      Pconst_integer (c, o) |> pexp_constant |> fun e ->
      eapply (evar "Z.of_int") [ e ]
  | Pconst_char c -> echar c
  | Pconst_string (s, d) -> Pconst_string (s, Location.none, d) |> pexp_constant
  | Pconst_float (c, o) -> Pconst_float (c, o) |> pexp_constant

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

let location_of_gospel_loc : Gospel.Warnings.loc option -> location = function
  | Some l ->
      { loc_start = l.loc_start; loc_end = l.loc_end; loc_ghost = l.loc_ghost }
  | None -> Location.none

let failed error_kind term_kind fun_name term =
  let func, exn =
    match error_kind with
    | `Violated -> ("violated", None)
    | `RuntimeExn e -> ("runtime_exn", Some e)
  in
  let loc =
    pexp_open
      (open_infos
         ~expr:(pmod_ident (noloc (lident "Ppxlib.Location")))
         ~override:Fresh)
      (elocation (location_of_gospel_loc term.Gospel.Tterm.t_loc))
  in
  let term =
    pexp_construct
      (noloc (lident (match term_kind with `Pre -> "Pre" | `Post -> "Post")))
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

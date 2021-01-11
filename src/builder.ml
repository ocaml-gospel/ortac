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

let failed_pre fun_name term =
  eapply (evar "violated")
    [
      pexp_open
        (open_infos
           ~expr:(pmod_ident (noloc (lident "Ppxlib.Location")))
           ~override:Fresh)
        (elocation (location_of_gospel_loc term.Gospel.Tterm.t_loc));
      estring fun_name;
      pexp_construct
        (noloc (lident "Pre"))
        (Some (estring (Fmt.str "%a" Gospel.Tterm.print_term term)));
    ]

let failed_post fun_name term =
  eapply (evar "violated")
    [
      pexp_open
        (open_infos
           ~expr:(pmod_ident (noloc (lident "Ppxlib.Location")))
           ~override:Fresh)
        (elocation (location_of_gospel_loc term.Gospel.Tterm.t_loc));
      estring fun_name;
      pexp_construct
        (noloc (lident "Post"))
        (Some (estring (Fmt.str "%a" Gospel.Tterm.print_term term)));
    ]

let failed_post_nonexec fun_name term exn =
  eapply (evar "runtime_exn")
    [
      pexp_open
        (open_infos
           ~expr:(pmod_ident (noloc (lident "Ppxlib.Location")))
           ~override:Fresh)
        (elocation (location_of_gospel_loc term.Gospel.Tterm.t_loc));
      estring fun_name;
      pexp_construct
        (noloc (lident "Post"))
        (Some (estring (Fmt.str "%a" Gospel.Tterm.print_term term)));
      exn;
    ]

let failed_pre_nonexec fun_name term exn =
  eapply (evar "runtime_exn")
    [
      pexp_open
        (open_infos
           ~expr:(pmod_ident (noloc (lident "Ppxlib.Location")))
           ~override:Fresh)
        (elocation (location_of_gospel_loc term.Gospel.Tterm.t_loc));
      estring fun_name;
      pexp_construct
        (noloc (lident "Pre"))
        (Some (estring (Fmt.str "%a" Gospel.Tterm.print_term term)));
      exn;
    ]

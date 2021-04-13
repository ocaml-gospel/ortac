open Ppxlib

include Ast_builder.Make (struct
  let loc = Location.none
end)

let noloc txt = { txt; loc = Location.none }

let epred e = eapply (evar "Z.pred") [ e ]

let esucc e = eapply (evar "Z.succ") [ e ]

let econst = function
  | Pconst_integer (c, o) ->
      Pconst_integer (c, o) |> pexp_constant |> fun e ->
      eapply (evar "Z.of_int") [ e ]
  | _ as e -> pexp_constant e

let eposition pos =
  [%expr
    {
      pos_fname = [%e estring pos.pos_fname];
      pos_lnum = [%e eint pos.pos_lnum];
      pos_bol = [%e eint pos.pos_bol];
      pos_cnum = [%e eint pos.pos_cnum];
    }]

let elocation loc =
  [%expr
    let open Ppxlib.Location in
    {
      loc_start = [%e eposition loc.loc_start];
      loc_end = [%e eposition loc.loc_end];
      loc_ghost = [%e ebool loc.loc_ghost];
    }]

let failed error_kind term_kind acc_name fun_name (eloc : expression) term :
    expression =
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
  match error_kind with
  | `Violated ->
      [%expr
        let err =
          mk_condition [%e eloc] [%e estring fun_name] [%e term] Violated
        in
        Errors.register err [%e evar acc_name]]
  | `RuntimeExn e ->
      [%expr
        let err =
          mk_condition [%e eloc] [%e estring fun_name] [%e term]
            (RuntimeExn [%e e])
        in
        Errors.register err [%e evar acc_name];
        true]

let failed_pre = failed `Violated `Pre

let failed_post = failed `Violated `Post

let failed_pre_nonexec acc_name exn = failed (`RuntimeExn exn) `Pre acc_name

let failed_post_nonexec acc_name exn = failed (`RuntimeExn exn) `Post acc_name

let failed_xpost = failed `Violated `XPost

let failed_xpost_nonexec acc_name exn = failed (`RuntimeExn exn) `XPost acc_name

let efun args expr =
  List.fold_right
    (fun a ->
      let label, p = a in
      pexp_fun label None p)
    args expr

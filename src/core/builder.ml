open Ppxlib

include Ast_builder.Make (struct
  let loc = Location.none
end)

let noloc txt = { txt; loc = Location.none }
let lident s = noloc (lident s)

let qualify ms v =
  let lid =
    match ms with
    | [] -> Lident v
    | m :: ms ->
        let pref = List.fold_left (fun acc x -> Ldot (acc, x)) (Lident m) ms in
        Ldot (pref, v)
  in
  Ast_helper.Exp.ident (noloc lid)

let epred e =
  let f = qualify [ "Ortac_runtime"; "Gospelstdlib" ] "pred" in
  eapply f [ e ]

let esucc e =
  let f = qualify [ "Ortac_runtime"; "Gospelstdlib" ] "succ" in
  eapply f [ e ]

let enot e = eapply (pexp_ident (lident "not")) [ e ]

let econst = function
  | Pconst_integer (c, o) ->
      Pconst_integer (c, o) |> pexp_constant |> fun e ->
      let f = qualify [ "Ortac_runtime"; "Gospelstdlib" ] "integer_of_int" in
      eapply f [ e ]
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
    {
      Ortac_runtime.start = [%e eposition loc.loc_start];
      Ortac_runtime.stop = [%e eposition loc.loc_end];
    }]

let efun args expr =
  List.fold_right
    (fun a ->
      let label, p = a in
      pexp_fun label None p)
    args expr

let list_fold_right1 op v xs =
  let rec aux = function
    | [ x ] -> x
    | x :: xs -> op x (aux xs)
    | _ -> failwith "The impossible happened in list_fold_right1"
  in
  match xs with [] -> v | _ -> aux xs

let list_and xs =
  let ( &&& ) e1 e2 =
    pexp_apply (pexp_ident (lident "&&")) [ (Nolabel, e1); (Nolabel, e2) ]
  and etrue = pexp_construct (lident "true") None in
  list_fold_right1 ( &&& ) etrue xs

let list_or xs =
  let ( ||| ) e1 e2 =
    pexp_apply (pexp_ident (lident "||")) [ (Nolabel, e1); (Nolabel, e2) ]
  and efalse = pexp_construct (lident "false") None in
  list_fold_right1 ( ||| ) efalse xs

open Ppxlib

include Ast_builder.Make (struct
  let loc = Location.none
end)

let noloc txt = { txt; loc = Location.none }
let epred e = eapply (evar "Ortac_runtime.Z.pred") [ e ]
let esucc e = eapply (evar "Ortac_runtime.Z.succ") [ e ]

let econst = function
  | Pconst_integer (c, o) ->
      Pconst_integer (c, o) |> pexp_constant |> fun e ->
      eapply (evar "Ortac_runtime.Z.of_int") [ e ]
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

let lident s = noloc (lident s)

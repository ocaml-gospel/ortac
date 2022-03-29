open Gospel.Tast
open Derive

let xpost xp map =
  let aux map (_, xs) =
    List.fold_left (fun map (_p, term) -> traverse term map) map xs
  in
  List.fold_left aux map xp

let value ~driver vd =
  match vd.vd_spec with
  | None -> driver
  | Some spec ->
      let f map =
        map
        |> fold_traverse spec.sp_checks
        |> fold_traverse spec.sp_pre
        |> fold_traverse spec.sp_post
        |> xpost spec.sp_xpost
        |> fold_traverse spec.sp_cs
        |> fold_traverse spec.sp_wr
      in
      Drv.map_repr ~f driver

let type_ ~driver td =
  match td.td_spec with
  | None -> driver
  | Some spec ->
      let f map = fold_traverse spec.ty_invariants map in
      Drv.map_repr ~f driver

let types ~driver = List.fold_left (fun driver -> type_ ~driver) driver

let function_ ~driver func =
  let driver =
    match func.fun_def with
    | None -> driver
    | Some def -> Drv.map_repr ~f:(traverse def) driver
  in
  match func.fun_spec with
  | None -> driver
  | Some spec ->
      let open Derive in
      let f map =
        map
        |> fold_traverse spec.fun_req
        |> fold_traverse spec.fun_ens
        |> fold_traverse spec.fun_variant
      in
      Drv.map_repr ~f driver

let axiom ~driver ax = Drv.map_repr ~f:(fold_traverse [ ax.ax_term ]) driver

let preprocess ~driver s : Drv.t =
  List.fold_left
    (fun driver (sig_item : signature_item) ->
      match sig_item.sig_desc with
      | Sig_val (vd, _) -> value ~driver vd
      | Sig_type (_, td, _) -> types ~driver td
      | Sig_function func -> function_ ~driver func
      | Sig_axiom ax -> axiom ~driver ax
      | _ -> driver)
    driver s

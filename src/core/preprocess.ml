open Gospel

let value ~driver vd =
  match vd.Tast.vd_spec with
  | None -> driver
  | Some spec ->
      let f map =
        map
        |> Derive.fold_traverse spec.sp_checks
        |> Derive.fold_traverse spec.sp_pre
        |> Derive.fold_traverse spec.sp_post
        (* |> Derive.fold_traverse_xpost spec.sp_xpost *)
        |> Derive.fold_traverse spec.sp_cs
        |> Derive.fold_traverse spec.sp_wr
      in
      Drv.map_repr ~f driver

let type_ ~driver _td = driver
let function_ ~driver _func = driver
let axiom ~driver _ax = driver

let preprocess ~driver s : Drv.t =
  List.fold_left
    (fun driver (sig_item : Tast.signature_item) ->
      match sig_item.sig_desc with
      | Sig_val (vd, _) -> value ~driver vd
      | Sig_type (_, td, _) -> type_ ~driver td
      | Sig_function func -> function_ ~driver func
      | Sig_axiom ax -> axiom ~driver ax
      | _ -> driver)
    driver s

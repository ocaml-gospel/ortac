open Gospel

module Mutability = struct
  let max m n =
    let open Translated in
    match (m, n) with
    | Mutable, _ | _, Mutable -> Mutable
    | Dependant f, _ | _, Dependant f -> Dependant f
    | Unknown, _ | _, Unknown -> Unknown
    | Immutable, Immutable -> Immutable

  let min_mut = Translated.Immutable

  let tysymbol ~driver (ts : Ttypes.tysymbol) =
    Option.fold ~none:Translated.Unknown
      ~some:(fun (t : Translated.type_) -> t.mutable_)
      (Drv.get_type ts driver)

  let alpha (ty : Ttypes.ty) =
    match ty.ty_node with Tyvar _ -> true | _ -> false

  let rec ty ~driver (t : Ttypes.ty) =
    match t.ty_node with
    | Tyvar _ -> Translated.Unknown
    | Tyapp (ts, tyl) when Ttypes.is_ts_tuple ts ->
        List.map (ty ~driver) tyl |> List.fold_left max min_mut
    | Tyapp (ts, tyl) -> (
        match tysymbol ~driver ts with
        | Translated.Dependant f as m ->
            if List.exists alpha tyl (* we still can't determine mutability *)
            then m
            else f (List.map (ty ~driver) tyl)
        | m -> m)

  let lsymbol ~driver (ls : Tterm.lsymbol) =
    Option.fold ~none:Translated.Unknown ~some:(ty ~driver) ls.ls_value

  let mutable_flag = function
    | Tast.Immutable -> Translated.Immutable
    | Tast.Mutable -> Translated.Mutable

  let constructor_declaration ~driver (cd : Tast.constructor_decl) =
    List.map (ty ~driver) cd.cd_cs.ls_args |> List.fold_left max min_mut

  let rec_declaration ~driver (rd : Tast.rec_declaration) =
    List.map
      (fun (ld : Tterm.lsymbol Tast.label_declaration) ->
        max (mutable_flag ld.ld_mut) (lsymbol ~driver ld.ld_field))
      rd.rd_ldl
    |> List.fold_left max min_mut

  let type_declaration ~driver (td : Tast.type_declaration) =
    match td.td_ts.ts_alias with
    | Some alias ->
        (* type t = int or type t = int array *)
        ty ~driver alias
    | None -> (
        match td.td_kind with
        | Pty_abstract -> Translated.Unknown (* type t *)
        | Pty_variant cdl ->
            List.map (constructor_declaration ~driver) cdl
            |> List.fold_left max min_mut
        | Pty_record rd -> rec_declaration ~driver rd
        | Pty_open -> Translated.Unknown (* ? *))

  let mutable_model ~driver (ty_fields : (Tterm.lsymbol * bool) list) =
    List.map
      (fun (ls, b) -> if b then Translated.Mutable else lsymbol ~driver ls)
      ty_fields
    |> List.fold_left max min_mut

  let type_spec ~driver (spec : Tast.type_spec) =
    if spec.ty_ephemeral then Translated.Mutable
    else mutable_model ~driver spec.ty_fields
end

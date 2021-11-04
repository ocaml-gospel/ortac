open Gospel

module Mutability = struct
  let known_tysymbol ~driver (ts : Ttypes.tysymbol) =
    Option.fold ~none:Translated.Unknown
      ~some:(fun (t : Translated.type_) -> t.mutable_)
      (Drv.get_type ts driver)

  let tysymbol ~driver (ts : Ttypes.tysymbol) =
    max
      (if
       Ttypes.ts_equal ts (Drv.get_ts driver [ "Gospelstdlib"; "array" ])
       || Ttypes.ts_equal ts (Drv.get_ts driver [ "Gospelstdlib"; "ref" ])
      then Translated.Mutable
      else Translated.Immutable)
      (known_tysymbol ~driver ts)

  let rec ty ~driver (t : Ttypes.ty) =
    match t.ty_node with
    | Tyvar _ -> Translated.Unknown
    | Tyapp (ts, tyl) ->
        List.fold_left
          (fun acc t -> max acc (ty ~driver t))
          (tysymbol ~driver ts) tyl

  let lsymbol ~driver (ls : Tterm.lsymbol) =
    Option.fold ~none:Translated.Unknown ~some:(ty ~driver) ls.ls_value
    |> List.fold_right (fun t acc -> max acc (ty ~driver t)) ls.ls_args

  let mutable_flag = function
    | Tast.Immutable -> Translated.Immutable
    | Tast.Mutable -> Translated.Mutable

  let constructor_declaration ~driver (cd : Tast.constructor_decl) =
    List.map
      (fun (ld : (Identifier.Ident.t * Ttypes.ty) Tast.label_declaration) ->
        max (mutable_flag ld.ld_mut) (ty ~driver (snd ld.ld_field)))
      cd.cd_ld
    |> List.fold_left max (lsymbol ~driver cd.cd_cs)

  let rec_declaration ~driver (rd : Tast.rec_declaration) =
    List.map
      (fun (ld : Tterm.lsymbol Tast.label_declaration) ->
        max (mutable_flag ld.ld_mut) (lsymbol ~driver ld.ld_field))
      rd.rd_ldl
    |> List.fold_left max (lsymbol ~driver rd.rd_cs)

  let type_declaration ~driver (td : Tast.type_declaration) =
    max
      (Option.fold ~none:Translated.Unknown ~some:(ty ~driver) td.td_ts.ts_alias)
      (match td.td_kind with
      | Pty_abstract -> Translated.Unknown
      | Pty_variant cdl ->
          List.map (constructor_declaration ~driver) cdl
          |> List.fold_left max Translated.Unknown
      | Pty_record rd -> rec_declaration ~driver rd
      | Pty_open -> Translated.Unknown)

  let inject_bool = function
    | true -> Translated.Mutable
    | false -> Translated.Immutable

  let mutable_model ~driver (ty_fields : (Tterm.lsymbol * bool) list) =
    List.map (fun (ls, b) -> max (inject_bool b) (lsymbol ~driver ls)) ty_fields
    |> List.fold_left max Translated.Unknown
end

open Gospel

module Mutability = struct
  let known_tysymbol ~driver (ts : Ttypes.tysymbol) =
    match Drv.get_type ts driver with
    | None -> false
    | Some type_ -> type_.mutable_

  let rec ty ~driver (t : Ttypes.ty) =
    match t.ty_node with
    | Tyvar _ -> false
    | Tyapp (ts, tyl) ->
        (* already known as mutable, an array, a reference or a container of something mutable *)
        known_tysymbol ~driver ts
        || Ttypes.ts_equal ts (Drv.get_ts driver [ "Gospelstdlib"; "array" ])
        || Ttypes.ts_equal ts (Drv.get_ts driver [ "Gospelstdlib"; "ref" ])
        || List.exists (ty ~driver) tyl

  let lsymbol ~driver (ls : Tterm.lsymbol) =
    (match ls.ls_value with None -> false | Some t -> ty ~driver t)
    || List.exists (ty ~driver) ls.ls_args

  let constructor_declaration ~driver (cd : Tast.constructor_decl) =
    List.exists
      (fun (ld : (Identifier.Ident.t * Ttypes.ty) Tast.label_declaration) ->
        ld.ld_mut = Mutable || ty ~driver (snd ld.ld_field))
      cd.cd_ld
    || lsymbol ~driver cd.cd_cs

  let rec_declaration ~driver (rd : Tast.rec_declaration) =
    List.exists
      (fun (ld : Tterm.lsymbol Tast.label_declaration) ->
        ld.ld_mut = Mutable || lsymbol ~driver ld.ld_field)
      rd.rd_ldl
    || lsymbol ~driver rd.rd_cs

  let type_declaration ~driver (td : Tast.type_declaration) =
    Option.fold ~none:false ~some:(ty ~driver) td.td_ts.ts_alias
    ||
    match td.td_kind with
    | Pty_abstract -> false
    | Pty_variant cdl -> List.exists (constructor_declaration ~driver) cdl
    | Pty_record rd -> rec_declaration ~driver rd
    | Pty_open -> false

  let mutable_model ~driver (ty_fields : (Tterm.lsymbol * bool) list) =
    List.exists (fun (ls, b) -> b || lsymbol ~driver ls) ty_fields
end

let ty_ident (ty : Gospel.Ttypes.ty) =
  match ty.ty_node with
  | Tyvar tvs -> tvs.tv_name.id_str
  | Tyapp (tys, _params) -> tys.ts_ident.id_str

let ty_params (ty : Gospel.Ttypes.ty) =
  match ty.ty_node with Tyvar _tvs -> [] | Tyapp (_tys, params) -> params

let get_ld_ident (ld : Gospel.Tterm.lsymbol Gospel.Tast.label_declaration) =
  match ld.ld_field.ls_value with
  | Some ty -> ty_ident ty
  | None -> failwith "can't find type"

let get_ty_params (ld : Gospel.Tterm.lsymbol Gospel.Tast.label_declaration) =
  match ld.ld_field.ls_value with
  | Some ty -> ty_params ty
  | None -> failwith "can't find type"

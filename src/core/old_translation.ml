module W = Warnings
open Ppxlib
open Gospel
open Builder
module S = Set.Make (Tterm.Vs)

let get_old_vs t =
  let rec aux ~in_old (acc : 'a) (t : Tterm.term) : 'a =
    match t.t_node with
    | Tterm.Tvar vs -> if in_old then S.add vs acc else acc
    | Tterm.Tconst _ | Tterm.Ttrue | Tterm.Tfalse -> acc
    | Tterm.Tnot t1 | Tterm.Tfield (t1, _) | Tterm.Tquant (_, _, _, t1) ->
        aux ~in_old acc t1
    | Tterm.Told t1 -> aux ~in_old:true acc t1
    | Tterm.Tlet (_, t1, t2) | Tterm.Tbinop (_, t1, t2) ->
        aux ~in_old (aux ~in_old acc t2) t1
    | Tterm.Tapp (_ls, tl1) ->
        List.fold_left (fun acc t -> S.union (aux ~in_old acc t) acc) acc tl1
    | Tterm.Tif (t1, t2, t3) ->
        aux ~in_old (aux ~in_old (aux ~in_old acc t3) t2) t1
    | Tterm.Tcase (t, ptl) ->
        List.fold_left
          (fun acc (_, t) -> aux ~in_old acc t)
          (aux ~in_old acc t) ptl
  in
  aux ~in_old:false S.empty t

let rec copy_ty ~driver ty =
  let open Ttypes in
  let ts_int = Drv.get_ts driver [ "Gospelstdlib"; "int" ] in
  let ts_array = Drv.get_ts driver [ "Gospelstdlib"; "array" ] in
  match ty.ty_node with
  | Ttypes.Tyvar _ -> None
  | Ttypes.Tyapp (ts, [])
    when ts_equal ts ts_unit || ts_equal ts ts_integer || ts_equal ts ts_bool
         || ts_equal ts ts_float || ts_equal ts ts_char || ts_equal ts ts_int ->
      [%expr Fun.id] |> Option.some
  | Ttypes.Tyapp (ts, []) when ts_equal ts ts_string ->
      [%expr String.copy] |> Option.some
  | Ttypes.Tyapp (ts, [ ty ]) when ts_equal ts ts_option ->
      Option.map
        (fun copy_ty -> [%expr Option.map [%e copy_ty]])
        (copy_ty ~driver ty)
  | Ttypes.Tyapp (ts, [ ty ]) when ts_equal ts ts_list ->
      Option.map
        (fun copy_ty -> [%expr List.map [%e copy_ty]])
        (copy_ty ~driver ty)
  | Ttypes.Tyapp (ts, [ ty ]) when ts_equal ts ts_array ->
      Option.map
        (fun copy_ty -> [%expr Array.map [%e copy_ty]])
        (copy_ty ~driver ty)
  | Ttypes.Tyapp (ts, tyl) ->
      let def = Drv.get_type_definition driver ts in
      let copy = copy_from_def ~driver def in
      let copy_tyl = List.filter_map (copy_ty ~driver) tyl in
      if List.length copy_tyl <> List.length tyl then None
      else Option.map (fun copy -> eapply copy copy_tyl) copy

and copy_from_def ~driver = function
  | Tast.Pty_abstract -> None
  | Tast.Pty_variant cl -> copy_constr ~driver cl
  | Tast.Pty_record r -> copy_record ~driver r
  | Tast.Pty_open -> None

(* TODO *)
and copy_constr ~driver:_ _cl = None

and copy_record ~driver (r : Tast.rec_declaration) =
  let var = gen_symbol () in
  let new_record_fields =
    List.filter_map
      (fun Tast.{ ld_field; ld_mut; _ } ->
        let field_ident = lident ld_field.Tterm.ls_name.id_str in
        let projection = pexp_field (evar var) field_ident in
        if ld_mut = Immutable then (field_ident, projection) |> Option.some
        else
          let out_ty = Option.get ld_field.Tterm.ls_value in
          Option.map
            (fun c -> (field_ident, eapply c [ projection ]))
            (copy_ty ~driver out_ty))
      r.rd_ldl
  in
  if List.length new_record_fields <> List.length r.rd_ldl then None
  else
    let new_record = pexp_record new_record_fields None in
    Some [%expr fun [%p pvar var] -> [%e new_record]]

let copy_vs ~driver ({ vs_name; vs_ty } : Tterm.vsymbol) =
  Option.map
    (fun copy_ty ->
      eapply copy_ty [ evar (Fmt.str "%a" Identifier.Ident.pp vs_name) ])
    (copy_ty ~driver vs_ty)

let process_olds ~driver terms =
  let olds = Hashtbl.create 0 in
  List.iter
    (fun t ->
      let vs = get_old_vs t in
      S.iter
        (fun (vs : Tterm.vsymbol) ->
          let old_vs =
            gen_symbol ~prefix:("__old_" ^ vs.vs_name.id_str) ()
            |> Identifier.Ident.create
            |> fun vs_name -> { vs with vs_name }
          in
          Hashtbl.add olds vs old_vs)
        vs)
    terms;
  ( olds,
    fun next ->
      Hashtbl.fold
        (fun vs (old_vs : Tterm.vsymbol) acc ->
          let old_name = Fmt.str "%a" Identifier.Ident.pp old_vs.vs_name in
          copy_vs ~driver vs |> function
          | None ->
              W.(register (Unsupported_old_copy vs.vs_name.id_str, loc));
              acc
          | Some new_vs ->
              Hashtbl.add olds vs old_vs;
              [%expr
                let [%p pvar old_name] = [%e new_vs] in
                [%e acc]])
        olds next )
